/*
 * emacs-test-runner - run Google Test cases in parallel on behalf of Emacs.
 *
 * Emacs starts one runner per batch and talks to it over stdin/stdout with a
 * line-based protocol described in ../README.md.  The runner discovers the
 * test cases, runs them from worker threads that each own at most one child
 * process, and redirects every child's output to a file in the output
 * directory.  Emacs therefore needs a single pipe however many tests run in
 * parallel.  Child processes are killed when the runner stops or dies.
 *
 * With a timing cache, runs record every test's duration from the Google Test
 * XML and save the test list and durations to the cache.  Later runs skip
 * discovery, read the cache and balance the tests across the threads.
 */

#if defined(_WIN32)
#include <windows.h>
#include <fcntl.h>
#include <io.h>
#include <process.h>
#else
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#if defined(__linux__)
#include <sys/prctl.h>
#endif
#endif

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RUNNER_NAME "emacs-test-runner"
#define RUNNER_VERSION "1.2"
#define PROTOCOL_VERSION "3"
#define MAX_THREADS 1024u

#if defined(_WIN32)
#define PATH_SEP "\\"
/* CreateProcess accepts at most 32767 characters including the NUL. */
#define COMMAND_LINE_LIMIT 32000u
#else
#define PATH_SEP "/"
/* Linux limits a single argument to 128 KiB. */
#define COMMAND_LINE_LIMIT 100000u
#endif

#if defined(__GNUC__) || defined(__clang__)
#define PRINTF_LIKE(fmt, args) __attribute__((format(printf, fmt, args)))
#define NORETURN __attribute__((noreturn))
#else
#define PRINTF_LIKE(fmt, args)
#define NORETURN __declspec(noreturn)
#endif

/* Memory and strings. */

NORETURN static void die(const char *message)
{
  fprintf(stderr, RUNNER_NAME ": %s\n", message);
  fflush(stderr);
  exit(2);
}

static void *xmalloc(size_t size)
{
  void *data = malloc(size ? size : 1);
  if (!data)
    die("out of memory");
  return data;
}

static void *xrealloc(void *data, size_t size)
{
  void *grown = realloc(data, size ? size : 1);
  if (!grown)
    die("out of memory");
  return grown;
}

static char *xstrdup(const char *text)
{
  size_t size = strlen(text) + 1;
  char *copy = xmalloc(size);
  memcpy(copy, text, size);
  return copy;
}

typedef struct {
  char *data;
  size_t len;
  size_t cap;
} strbuf;

static void sb_reserve(strbuf *sb, size_t extra)
{
  if (sb->len + extra + 1 > sb->cap) {
    size_t cap = sb->cap ? sb->cap : 128;
    while (sb->len + extra + 1 > cap)
      cap *= 2;
    sb->data = xrealloc(sb->data, cap);
    sb->cap = cap;
  }
}

static void sb_addn(strbuf *sb, const char *text, size_t len)
{
  sb_reserve(sb, len);
  memcpy(sb->data + sb->len, text, len);
  sb->len += len;
  sb->data[sb->len] = '\0';
}

static void sb_add(strbuf *sb, const char *text)
{
  sb_addn(sb, text, strlen(text));
}

static void sb_addc(strbuf *sb, char c)
{
  sb_addn(sb, &c, 1);
}

PRINTF_LIKE(2, 3) static void sb_addf(strbuf *sb, const char *format, ...)
{
  va_list args;
  int needed;

  va_start(args, format);
  needed = vsnprintf(NULL, 0, format, args);
  va_end(args);
  if (needed < 0)
    return;
  sb_reserve(sb, (size_t)needed);
  va_start(args, format);
  vsnprintf(sb->data + sb->len, (size_t)needed + 1, format, args);
  va_end(args);
  sb->len += (size_t)needed;
}

static void sb_clear(strbuf *sb)
{
  sb->len = 0;
  if (sb->data)
    sb->data[0] = '\0';
}

static void sb_free(strbuf *sb)
{
  free(sb->data);
  sb->data = NULL;
  sb->len = sb->cap = 0;
}

/* Protocol fields cannot contain tabs or line breaks. */
static void sb_add_field(strbuf *sb, const char *text)
{
  sb_addc(sb, '\t');
  for (; *text; ++text)
    sb_addc(sb, (*text == '\t' || *text == '\n' || *text == '\r') ? ' ' : *text);
}

typedef struct {
  char **items;
  size_t count;
  size_t cap;
} strvec;

static void sv_push(strvec *vec, char *item)
{
  if (vec->count == vec->cap) {
    vec->cap = vec->cap ? vec->cap * 2 : 16;
    vec->items = xrealloc(vec->items, vec->cap * sizeof *vec->items);
  }
  vec->items[vec->count++] = item;
}

static void sv_free_items(strvec *vec)
{
  for (size_t i = 0; i < vec->count; ++i)
    free(vec->items[i]);
  free(vec->items);
  vec->items = NULL;
  vec->count = vec->cap = 0;
}

/* Platform layer. */

typedef struct {
  int started;
  int retryable; /* Start failed for lack of resources. */
  unsigned long exit_code;
  uint64_t elapsed_us; /* Wall time of the last start attempt. */
  char message[512];
} proc_result;

static void child_started(void);
static void child_exited(void);

#if defined(_WIN32)

typedef CRITICAL_SECTION mutex_t;
typedef CONDITION_VARIABLE cond_t;

static void mutex_init(mutex_t *mutex) { InitializeCriticalSection(mutex); }
static void mutex_lock(mutex_t *mutex) { EnterCriticalSection(mutex); }
static void mutex_unlock(mutex_t *mutex) { LeaveCriticalSection(mutex); }
static void cond_init(cond_t *cond) { InitializeConditionVariable(cond); }
static void cond_wait(cond_t *cond, mutex_t *mutex)
{
  SleepConditionVariableCS(cond, mutex, INFINITE);
}
static void cond_broadcast(cond_t *cond) { WakeAllConditionVariable(cond); }

static HANDLE job_handle;
static HANDLE null_handle;
static uint64_t counter_frequency;

static wchar_t *utf8_to_wide(const char *text)
{
  int count = MultiByteToWideChar(CP_UTF8, 0, text, -1, NULL, 0);
  wchar_t *wide;

  if (count <= 0)
    return NULL;
  wide = xmalloc((size_t)count * sizeof *wide);
  MultiByteToWideChar(CP_UTF8, 0, text, -1, wide, count);
  return wide;
}

static char *wide_to_utf8(const wchar_t *wide)
{
  int count = WideCharToMultiByte(CP_UTF8, 0, wide, -1, NULL, 0, NULL, NULL);
  char *text;

  if (count <= 0)
    return xstrdup("");
  text = xmalloc((size_t)count);
  WideCharToMultiByte(CP_UTF8, 0, wide, -1, text, count, NULL, NULL);
  return text;
}

static void windows_error(char *buffer, size_t size, const char *what, DWORD code)
{
  wchar_t *wide = NULL;
  char *text;
  size_t len;

  FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM
                   | FORMAT_MESSAGE_IGNORE_INSERTS,
                 NULL, code, 0, (LPWSTR)&wide, 0, NULL);
  text = wide ? wide_to_utf8(wide) : xstrdup("unknown error");
  len = strlen(text);
  while (len > 0 && (text[len - 1] == '\r' || text[len - 1] == '\n'
                     || text[len - 1] == ' ' || text[len - 1] == '.'))
    text[--len] = '\0';
  snprintf(buffer, size, "%s failed: %s (error %lu)", what, text,
           (unsigned long)code);
  free(text);
  LocalFree(wide);
}

static int windows_resource_error(DWORD code)
{
  switch (code) {
  case ERROR_TOO_MANY_OPEN_FILES:
  case ERROR_NOT_ENOUGH_MEMORY:
  case ERROR_OUTOFMEMORY:
  case ERROR_NO_SYSTEM_RESOURCES:
  case ERROR_NONPAGED_SYSTEM_RESOURCES:
  case ERROR_PAGED_SYSTEM_RESOURCES:
  case ERROR_WORKING_SET_QUOTA:
  case ERROR_PAGEFILE_QUOTA:
  case ERROR_COMMITMENT_LIMIT:
  case ERROR_NOT_ENOUGH_QUOTA:
  case ERROR_MAX_THRDS_REACHED:
    return 1;
  default:
    return 0;
  }
}

static void normalize_path(char *path)
{
  for (; *path; ++path)
    if (*path == '/')
      *path = '\\';
}

/* Quote ARG as parsed by CommandLineToArgvW and the MSVC runtime. */
static void sb_add_windows_arg(strbuf *sb, const char *arg)
{
  if (*arg && !strpbrk(arg, " \t\n\v\"")) {
    sb_add(sb, arg);
    return;
  }
  sb_addc(sb, '"');
  for (const char *p = arg;; ++p) {
    size_t backslashes = 0;
    while (*p == '\\') {
      ++p;
      ++backslashes;
    }
    if (*p == '\0') {
      for (size_t i = 0; i < backslashes * 2; ++i)
        sb_addc(sb, '\\');
      break;
    }
    if (*p == '"') {
      for (size_t i = 0; i < backslashes * 2 + 1; ++i)
        sb_addc(sb, '\\');
    } else {
      for (size_t i = 0; i < backslashes; ++i)
        sb_addc(sb, '\\');
    }
    sb_addc(sb, *p);
  }
  sb_addc(sb, '"');
}

static void platform_init(void)
{
  SECURITY_ATTRIBUTES inherit = { sizeof inherit, NULL, TRUE };
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
  LARGE_INTEGER frequency;

  _setmode(_fileno(stdin), _O_BINARY);
  _setmode(_fileno(stdout), _O_BINARY);
  QueryPerformanceFrequency(&frequency);
  counter_frequency = (uint64_t)frequency.QuadPart;

  job_handle = CreateJobObjectW(NULL, NULL);
  if (!job_handle)
    die("could not create a job object");
  ZeroMemory(&limits, sizeof limits);
  limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
  if (!SetInformationJobObject(job_handle, JobObjectExtendedLimitInformation,
                               &limits, sizeof limits))
    die("could not configure the job object");

  null_handle = CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                            &inherit, OPEN_EXISTING, 0, NULL);
  if (null_handle == INVALID_HANDLE_VALUE)
    die("could not open NUL");
}

NORETURN static void stop_all_and_exit(int code)
{
  fflush(stdout);
  TerminateJobObject(job_handle, 1);
  ExitProcess((UINT)code);
}

static void run_process(const strvec *argv, const char *cwd, const char *log_path,
                        proc_result *result)
{
  SECURITY_ATTRIBUTES inherit = { sizeof inherit, NULL, TRUE };
  strbuf command = { 0 };
  wchar_t *wide_command, *wide_cwd = NULL, *wide_log;
  HANDLE log = INVALID_HANDLE_VALUE;
  HANDLE inherited[2];
  LPPROC_THREAD_ATTRIBUTE_LIST attributes = NULL;
  SIZE_T attributes_size = 0;
  STARTUPINFOEXW startup;
  PROCESS_INFORMATION info;
  DWORD exit_code = 0;

  memset(result, 0, sizeof *result);
  for (size_t i = 0; i < argv->count; ++i) {
    if (i)
      sb_addc(&command, ' ');
    sb_add_windows_arg(&command, argv->items[i]);
  }
  wide_command = utf8_to_wide(command.data);
  wide_log = utf8_to_wide(log_path);
  if (cwd && *cwd)
    wide_cwd = utf8_to_wide(cwd);
  sb_free(&command);
  if (!wide_command || !wide_log) {
    snprintf(result->message, sizeof result->message, "invalid UTF-8 in command or path");
    goto done;
  }

  log = CreateFileW(wide_log, GENERIC_WRITE,
                    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, &inherit,
                    CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (log == INVALID_HANDLE_VALUE) {
    DWORD error = GetLastError();
    windows_error(result->message, sizeof result->message, "Creating log file", error);
    result->retryable = windows_resource_error(error);
    goto done;
  }

  inherited[0] = null_handle;
  inherited[1] = log;
  InitializeProcThreadAttributeList(NULL, 2, 0, &attributes_size);
  attributes = xmalloc(attributes_size);
  if (!InitializeProcThreadAttributeList(attributes, 2, 0, &attributes_size)) {
    windows_error(result->message, sizeof result->message,
                  "InitializeProcThreadAttributeList", GetLastError());
    free(attributes);
    attributes = NULL;
    goto done;
  }
  /* Inherit only this child's handles and create it inside the job, so it
     cannot outlive the runner even if the runner dies right after. */
  if (!UpdateProcThreadAttribute(attributes, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                                 inherited, sizeof inherited, NULL, NULL)
      || !UpdateProcThreadAttribute(attributes, 0, PROC_THREAD_ATTRIBUTE_JOB_LIST,
                                    &job_handle, sizeof job_handle, NULL, NULL)) {
    windows_error(result->message, sizeof result->message, "UpdateProcThreadAttribute",
                  GetLastError());
    goto done;
  }

  ZeroMemory(&startup, sizeof startup);
  startup.StartupInfo.cb = sizeof startup;
  startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  startup.StartupInfo.hStdInput = null_handle;
  startup.StartupInfo.hStdOutput = log;
  startup.StartupInfo.hStdError = log;
  startup.lpAttributeList = attributes;
  if (!CreateProcessW(NULL, wide_command, NULL, NULL, TRUE,
                      EXTENDED_STARTUPINFO_PRESENT | CREATE_NO_WINDOW
                        | CREATE_UNICODE_ENVIRONMENT,
                      NULL, wide_cwd, &startup.StartupInfo, &info)) {
    DWORD error = GetLastError();
    windows_error(result->message, sizeof result->message, "CreateProcess", error);
    result->retryable = windows_resource_error(error);
    goto done;
  }
  child_started();
  CloseHandle(info.hThread);
  CloseHandle(log);
  log = INVALID_HANDLE_VALUE;
  WaitForSingleObject(info.hProcess, INFINITE);
  GetExitCodeProcess(info.hProcess, &exit_code);
  CloseHandle(info.hProcess);
  child_exited();
  result->started = 1;
  result->exit_code = exit_code;

done:
  if (attributes) {
    DeleteProcThreadAttributeList(attributes);
    free(attributes);
  }
  if (log != INVALID_HANDLE_VALUE)
    CloseHandle(log);
  free(wide_command);
  free(wide_cwd);
  free(wide_log);
}

static int make_directory(const char *path)
{
  wchar_t *wide = utf8_to_wide(path);
  int ok = wide && (CreateDirectoryW(wide, NULL)
                    || GetLastError() == ERROR_ALREADY_EXISTS);
  free(wide);
  return ok;
}

static char *default_output_directory(void)
{
  wchar_t temp[MAX_PATH + 1];
  DWORD len = GetTempPathW(MAX_PATH + 1, temp);
  strbuf path = { 0 };
  char *text;

  if (len == 0 || len > MAX_PATH)
    return NULL;
  text = wide_to_utf8(temp);
  sb_add(&path, text);
  free(text);
  sb_addf(&path, RUNNER_NAME "-%lu", (unsigned long)GetCurrentProcessId());
  return path.data;
}

static FILE *open_for_reading(const char *path)
{
  wchar_t *wide = utf8_to_wide(path);
  FILE *file = wide ? _wfopen(wide, L"rb") : NULL;
  free(wide);
  return file;
}

static FILE *open_for_writing(const char *path)
{
  wchar_t *wide = utf8_to_wide(path);
  FILE *file = wide ? _wfopen(wide, L"wb") : NULL;
  free(wide);
  return file;
}

/* Atomically replace TO with FROM. */
static int replace_file(const char *from, const char *to)
{
  wchar_t *wide_from = utf8_to_wide(from);
  wchar_t *wide_to = utf8_to_wide(to);
  int ok = wide_from && wide_to
           && MoveFileExW(wide_from, wide_to, MOVEFILE_REPLACE_EXISTING);
  free(wide_from);
  free(wide_to);
  return ok;
}

static void delete_file(const char *path)
{
  wchar_t *wide = utf8_to_wide(path);
  if (wide)
    DeleteFileW(wide);
  free(wide);
}

static uint64_t monotonic_us(void)
{
  LARGE_INTEGER now;
  uint64_t ticks;

  QueryPerformanceCounter(&now);
  ticks = (uint64_t)now.QuadPart;
  return ticks / counter_frequency * 1000000u
         + ticks % counter_frequency * 1000000u / counter_frequency;
}

static unsigned __stdcall worker_entry(void *arg);

static int start_thread(void)
{
  uintptr_t thread = _beginthreadex(NULL, 0, worker_entry, NULL, 0, NULL);
  if (!thread)
    return 0;
  CloseHandle((HANDLE)thread);
  return 1;
}

#else /* POSIX */

typedef pthread_mutex_t mutex_t;
typedef pthread_cond_t cond_t;

static void mutex_init(mutex_t *mutex) { pthread_mutex_init(mutex, NULL); }
static void mutex_lock(mutex_t *mutex) { pthread_mutex_lock(mutex); }
static void mutex_unlock(mutex_t *mutex) { pthread_mutex_unlock(mutex); }
static void cond_init(cond_t *cond) { pthread_cond_init(cond, NULL); }
static void cond_wait(cond_t *cond, mutex_t *mutex) { pthread_cond_wait(cond, mutex); }
static void cond_broadcast(cond_t *cond) { pthread_cond_broadcast(cond); }

/* Held while creating descriptors and forking, so that no child inherits
   another child's pipe and stop can see every live child. */
static mutex_t spawn_lock;
static pid_t *live_pids;
static size_t live_count;
static size_t live_cap;

static void normalize_path(char *path) { (void)path; }

static void platform_init(void)
{
  mutex_init(&spawn_lock);
}

NORETURN static void stop_all_and_exit(int code)
{
  fflush(stdout);
  mutex_lock(&spawn_lock);
  for (size_t i = 0; i < live_count; ++i)
    kill(live_pids[i], SIGKILL);
  _exit(code);
}

NORETURN static void exec_child(char **argv, const char *cwd, int null_fd, int log_fd,
                                int error_fd)
{
  int error;
#if defined(__linux__)
  prctl(PR_SET_PDEATHSIG, SIGKILL);
#endif
  if ((cwd && *cwd && chdir(cwd) != 0) || dup2(null_fd, 0) < 0 || dup2(log_fd, 1) < 0
      || dup2(log_fd, 2) < 0)
    goto fail;
  execvp(argv[0], argv);
fail:
  error = errno;
  if (write(error_fd, &error, sizeof error) < 0)
    _exit(127);
  _exit(127);
}

static int posix_resource_error(int error)
{
  return error == EMFILE || error == ENFILE || error == ENOMEM || error == EAGAIN;
}

static void run_process(const strvec *argv, const char *cwd, const char *log_path,
                        proc_result *result)
{
  char **args = xmalloc((argv->count + 1) * sizeof *args);
  int log_fd = -1, null_fd = -1, fds[2] = { -1, -1 };
  int child_error = 0, status = 0;
  ssize_t got;
  pid_t pid;

  memset(result, 0, sizeof *result);
  for (size_t i = 0; i < argv->count; ++i)
    args[i] = argv->items[i];
  args[argv->count] = NULL;

  mutex_lock(&spawn_lock);
  log_fd = open(log_path, O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, 0644);
  if (log_fd < 0) {
    snprintf(result->message, sizeof result->message, "Creating log file %s failed: %s",
             log_path, strerror(errno));
    result->retryable = posix_resource_error(errno);
    goto unlock;
  }
  null_fd = open("/dev/null", O_RDONLY | O_CLOEXEC);
  if (null_fd < 0 || pipe(fds) != 0 || fcntl(fds[0], F_SETFD, FD_CLOEXEC) != 0
      || fcntl(fds[1], F_SETFD, FD_CLOEXEC) != 0) {
    snprintf(result->message, sizeof result->message, "Preparing child failed: %s",
             strerror(errno));
    result->retryable = posix_resource_error(errno);
    goto unlock;
  }
  pid = fork();
  if (pid == 0)
    exec_child(args, cwd, null_fd, log_fd, fds[1]);
  if (pid < 0) {
    snprintf(result->message, sizeof result->message, "fork failed: %s", strerror(errno));
    result->retryable = posix_resource_error(errno);
    goto unlock;
  }
  child_started();
  if (live_count == live_cap) {
    live_cap = live_cap ? live_cap * 2 : 16;
    live_pids = xrealloc(live_pids, live_cap * sizeof *live_pids);
  }
  live_pids[live_count++] = pid;
  mutex_unlock(&spawn_lock);

  close(fds[1]);
  fds[1] = -1;
  do
    got = read(fds[0], &child_error, sizeof child_error);
  while (got < 0 && errno == EINTR);
  while (waitpid(pid, &status, 0) < 0 && errno == EINTR)
    ;
  child_exited();

  mutex_lock(&spawn_lock);
  for (size_t i = 0; i < live_count; ++i)
    if (live_pids[i] == pid) {
      live_pids[i] = live_pids[--live_count];
      break;
    }
  if (got == (ssize_t)sizeof child_error) {
    snprintf(result->message, sizeof result->message, "Starting %s failed: %s", args[0],
             strerror(child_error));
    result->retryable = posix_resource_error(child_error);
  } else {
    result->started = 1;
    if (WIFEXITED(status))
      result->exit_code = (unsigned long)WEXITSTATUS(status);
    else if (WIFSIGNALED(status))
      result->exit_code = 128ul + (unsigned long)WTERMSIG(status);
    else
      result->exit_code = 1;
  }

unlock:
  mutex_unlock(&spawn_lock);
  if (log_fd >= 0)
    close(log_fd);
  if (null_fd >= 0)
    close(null_fd);
  if (fds[0] >= 0)
    close(fds[0]);
  if (fds[1] >= 0)
    close(fds[1]);
  free(args);
}

static int make_directory(const char *path)
{
  return mkdir(path, 0700) == 0 || errno == EEXIST;
}

static char *default_output_directory(void)
{
  const char *temp = getenv("TMPDIR");
  strbuf path = { 0 };

  sb_add(&path, temp && *temp ? temp : "/tmp");
  sb_addf(&path, "/" RUNNER_NAME "-%ld", (long)getpid());
  return path.data;
}

static FILE *open_for_reading(const char *path)
{
  return fopen(path, "rb");
}

static FILE *open_for_writing(const char *path)
{
  return fopen(path, "wb");
}

/* Atomically replace TO with FROM. */
static int replace_file(const char *from, const char *to)
{
  return rename(from, to) == 0;
}

static void delete_file(const char *path)
{
  (void)remove(path);
}

static uint64_t monotonic_us(void)
{
  struct timespec now;

  clock_gettime(CLOCK_MONOTONIC, &now);
  return (uint64_t)now.tv_sec * 1000000u + (uint64_t)now.tv_nsec / 1000u;
}

static void *worker_entry(void *arg);

static int start_thread(void)
{
  pthread_t thread;
  pthread_attr_t attributes;
  int ok;

  pthread_attr_init(&attributes);
  pthread_attr_setdetachstate(&attributes, PTHREAD_CREATE_DETACHED);
  ok = pthread_create(&thread, &attributes, worker_entry, NULL) == 0;
  pthread_attr_destroy(&attributes);
  return ok;
}

#endif

/* Read the file at PATH and NUL-terminate it.  Store its size in SIZE unless
   SIZE is NULL. */
static char *read_file(const char *path, size_t *size)
{
  FILE *file = open_for_reading(path);
  strbuf contents = { 0 };
  char chunk[65536];
  size_t got;

  if (!file)
    return NULL;
  sb_reserve(&contents, 0);
  contents.data[0] = '\0';
  while ((got = fread(chunk, 1, sizeof chunk, file)) > 0)
    sb_addn(&contents, chunk, got);
  fclose(file);
  if (size)
    *size = contents.len;
  return contents.data;
}

/* Live child count, used to retry starts that failed for lack of resources
   once another child has exited and released its resources. */

static struct {
  mutex_t lock;
  cond_t exited;
  unsigned running;
  unsigned long exits;
} children;

static void child_started(void)
{
  mutex_lock(&children.lock);
  ++children.running;
  mutex_unlock(&children.lock);
}

static void child_exited(void)
{
  mutex_lock(&children.lock);
  --children.running;
  ++children.exits;
  cond_broadcast(&children.exited);
  mutex_unlock(&children.lock);
}

static void run_process_retrying(const strvec *argv, const char *cwd,
                                 const char *log_path, proc_result *result)
{
  for (;;) {
    unsigned long exits;
    uint64_t started;

    mutex_lock(&children.lock);
    exits = children.exits;
    mutex_unlock(&children.lock);
    started = monotonic_us();
    run_process(argv, cwd, log_path, result);
    result->elapsed_us = monotonic_us() - started;
    if (result->started || !result->retryable)
      return;
    mutex_lock(&children.lock);
    if (children.running == 0 && children.exits == exits) {
      mutex_unlock(&children.lock);
      return;
    }
    while (children.exits == exits)
      cond_wait(&children.exited, &children.lock);
    mutex_unlock(&children.lock);
  }
}

/* Events written to Emacs. */

static mutex_t output_lock;

static void emit(strbuf *line)
{
  int failed;

  sb_addc(line, '\n');
  mutex_lock(&output_lock);
  failed = fwrite(line->data, 1, line->len, stdout) != line->len || fflush(stdout) != 0;
  mutex_unlock(&output_lock);
  if (failed)
    stop_all_and_exit(1);
  sb_clear(line);
}

static void emit_simple(const char *event, const char *field)
{
  strbuf line = { 0 };
  sb_add(&line, event);
  if (field)
    sb_add_field(&line, field);
  emit(&line);
  sb_free(&line);
}

static void emit_error(const char *message)
{
  emit_simple("error", message);
}

/* Configuration received from Emacs. */

static struct {
  char *exe;
  char *cwd;
  char *outdir;
  char *filter;
  char *cache;
  strvec args;
  strvec rerun_args;
  unsigned threads;
  int exclude_slow;
  int rediscover;
} config;

/* Work queue shared by the worker threads. */

typedef enum { JOB_DISCOVER, JOB_CHUNK, JOB_RERUN } job_kind;

typedef struct job {
  job_kind kind;
  unsigned id;
  char *rerun_id;
  char **names; /* Borrowed from selected_tests, except for reruns. */
  size_t name_count;
  uint64_t expected_us;
  struct job *next;
} job;

static struct {
  mutex_t lock;
  cond_t changed;
  job *head;
  job *tail;
  unsigned active;
  unsigned next_id;
  size_t chunks_left;
  int started;
  int quit_requested;
} queue;

static strvec all_tests;             /* Every listed test, in list order. */
static strvec selected_tests;        /* Borrowed from all_tests. */
static uint64_t *selected_durations; /* Parallel to selected_tests. */

static void enqueue_locked(job *item)
{
  item->next = NULL;
  item->id = ++queue.next_id;
  if (queue.tail)
    queue.tail->next = item;
  else
    queue.head = item;
  queue.tail = item;
}

static job *new_job(job_kind kind)
{
  job *item = xmalloc(sizeof *item);
  memset(item, 0, sizeof *item);
  item->kind = kind;
  return item;
}

static char *output_path(const char *prefix, unsigned id, const char *suffix)
{
  strbuf path = { 0 };
  sb_add(&path, config.outdir);
  sb_add(&path, PATH_SEP);
  sb_addf(&path, "%s-%u%s", prefix, id, suffix);
  return path.data;
}

static void base_argv(strvec *argv, const strvec *args)
{
  sv_push(argv, xstrdup(config.exe));
  for (size_t i = 0; i < args->count; ++i)
    sv_push(argv, xstrdup(args->items[i]));
}

/* Discovery output parsing, matching `--gtest_list_tests'. */

static int is_space(char c)
{
  return c == ' ' || c == '\t' || c == '\r' || c == '\v' || c == '\f';
}

/* Return the length of the name token at LINE if the rest of the line is
   only whitespace or a `#' comment, otherwise 0. */
static size_t list_token(const char *line, size_t len)
{
  size_t end = 0, rest;

  while (end < len && !is_space(line[end]) && line[end] != '#')
    ++end;
  rest = end;
  while (rest < len && is_space(line[rest]))
    ++rest;
  return (rest == len || line[rest] == '#') ? end : 0;
}

typedef struct {
  char **slots;
  size_t cap;
  size_t count;
} string_set;

static size_t hash_string(const char *text)
{
  size_t hash = 2166136261u;
  for (; *text; ++text)
    hash = (hash ^ (unsigned char)*text) * 16777619u;
  return hash;
}

/* Add TEXT to SET unless present; return non-zero if it was added. */
static int set_add(string_set *set, char *text)
{
  size_t index;

  if ((set->count + 1) * 2 > set->cap) {
    string_set grown = { 0 };
    grown.cap = set->cap ? set->cap * 2 : 256;
    grown.slots = xmalloc(grown.cap * sizeof *grown.slots);
    memset(grown.slots, 0, grown.cap * sizeof *grown.slots);
    for (size_t i = 0; i < set->cap; ++i)
      if (set->slots[i])
        set_add(&grown, set->slots[i]);
    free(set->slots);
    *set = grown;
  }
  index = hash_string(text) & (set->cap - 1);
  while (set->slots[index]) {
    if (strcmp(set->slots[index], text) == 0)
      return 0;
    index = (index + 1) & (set->cap - 1);
  }
  set->slots[index] = text;
  ++set->count;
  return 1;
}

static int suite_disabled(const char *suite, size_t len)
{
  for (size_t i = 0; i + 9 <= len; ++i)
    if ((i == 0 || suite[i - 1] == '/') && memcmp(suite + i, "DISABLED_", 9) == 0)
      return 1;
  return 0;
}

static void parse_test_list(char *output, strvec *tests)
{
  string_set seen = { 0 };
  const char *suite = NULL;
  size_t suite_len = 0;
  char *line = output;

  while (*line) {
    char *newline = strchr(line, '\n');
    size_t len = newline ? (size_t)(newline - line) : strlen(line);
    size_t token;

    if (len > 0 && !is_space(line[0]) && line[0] != '#') {
      token = list_token(line, len);
      if (token >= 2 && line[token - 1] == '.') {
        suite = line;
        suite_len = token;
      }
    } else if (suite && len > 2 && line[0] == ' ' && line[1] == ' '
               && !is_space(line[2]) && line[2] != '#') {
      token = list_token(line + 2, len - 2);
      if (token > 0 && !suite_disabled(suite, suite_len)
          && !(token >= 9 && memcmp(line + 2, "DISABLED_", 9) == 0)) {
        char *name = xmalloc(suite_len + token + 1);
        memcpy(name, suite, suite_len);
        memcpy(name + suite_len, line + 2, token);
        name[suite_len + token] = '\0';
        if (set_add(&seen, name))
          sv_push(tests, name);
        else
          free(name);
      }
    }
    if (!newline)
      break;
    line = newline + 1;
  }
  free(seen.slots);
}

static int is_slow(const char *name)
{
  for (const char *p = name; (p = strstr(p, "SLOW")) != NULL; ++p)
    if (p == name || p[-1] == '.' || p[-1] == '/')
      return 1;
  return 0;
}

static int is_selected(const char *name)
{
  if (config.exclude_slow && is_slow(name))
    return 0;
  return !config.filter || !*config.filter || strstr(name, config.filter) != NULL;
}

/* Name lookup into a fixed array of test names. */

#define NOT_FOUND SIZE_MAX

typedef struct {
  char **names;
  size_t *slots; /* Index into NAMES plus one, or 0 for an empty slot. */
  size_t mask;
} name_index;

static void index_build(name_index *index, char **names, size_t count)
{
  size_t cap = 16;

  while (cap < count * 2)
    cap *= 2;
  index->names = names;
  index->mask = cap - 1;
  index->slots = xmalloc(cap * sizeof *index->slots);
  memset(index->slots, 0, cap * sizeof *index->slots);
  for (size_t i = 0; i < count; ++i) {
    size_t slot = hash_string(names[i]) & index->mask;
    while (index->slots[slot] && strcmp(names[index->slots[slot] - 1], names[i]) != 0)
      slot = (slot + 1) & index->mask;
    if (!index->slots[slot])
      index->slots[slot] = i + 1;
  }
}

static size_t index_find(const name_index *index, const char *name)
{
  for (size_t slot = hash_string(name) & index->mask; index->slots[slot];
       slot = (slot + 1) & index->mask)
    if (strcmp(index->names[index->slots[slot] - 1], name) == 0)
      return index->slots[slot] - 1;
  return NOT_FOUND;
}

static void index_free(name_index *index)
{
  free(index->slots);
  index->slots = NULL;
}

/* Timing cache.  The file is a cache_header followed by COUNT durations,
   COUNT name offsets and NAMES_SIZE bytes of NUL-terminated names, so each
   array loads with a single memcpy.  Integers use the native byte order; a
   file from another byte order fails the version check and is rebuilt. */

#define CACHE_MAGIC "ETRCACHE"
#define CACHE_VERSION 1u
#define CACHE_MAX_TESTS 0x1000000u
#define DURATION_UNKNOWN UINT64_MAX
/* Larger durations (11.5 days) are treated as corrupt. */
#define DURATION_MAX_US 1000000000000u

typedef struct {
  char magic[8];
  uint32_t version;
  uint32_t count;
  uint64_t names_size;
  uint64_t overhead_us; /* Mean time per process outside the test bodies. */
} cache_header;

_Static_assert(sizeof(cache_header) == 32, "cache_header must not have padding");

typedef struct {
  char *data; /* The file contents; NAMES point into it. */
  char **names;
  uint64_t *durations;
  size_t count;
  uint64_t overhead_us;
} cache_data;

static void cache_free(cache_data *cache)
{
  free(cache->data);
  free(cache->names);
  free(cache->durations);
  memset(cache, 0, sizeof *cache);
}

/* Load the cache at PATH.  Return 1 on success, 0 if the file cannot be read
   and -1 if it is invalid. */
static int cache_load(const char *path, cache_data *cache)
{
  cache_header header;
  uint32_t *offsets = NULL;
  size_t size = 0, count;
  char *names;

  memset(cache, 0, sizeof *cache);
  cache->data = read_file(path, &size);
  if (!cache->data)
    return 0;
  if (size < sizeof header)
    goto invalid;
  memcpy(&header, cache->data, sizeof header);
  if (memcmp(header.magic, CACHE_MAGIC, sizeof header.magic) != 0
      || header.version != CACHE_VERSION || header.count > CACHE_MAX_TESTS
      || header.names_size > (uint64_t)size
      || sizeof header + (uint64_t)header.count * (sizeof(uint64_t) + sizeof(uint32_t))
             + header.names_size
           != (uint64_t)size
      || (header.count > 0
          && (header.names_size == 0 || cache->data[size - 1] != '\0')))
    goto invalid;

  count = header.count;
  names = cache->data + (size - (size_t)header.names_size);
  cache->durations = xmalloc(count * sizeof *cache->durations);
  memcpy(cache->durations, cache->data + sizeof header, count * sizeof *cache->durations);
  offsets = xmalloc(count * sizeof *offsets);
  memcpy(offsets, cache->data + sizeof header + count * sizeof *cache->durations,
         count * sizeof *offsets);
  cache->names = xmalloc(count * sizeof *cache->names);
  for (size_t i = 0; i < count; ++i) {
    if (offsets[i] >= header.names_size || names[offsets[i]] == '\0')
      goto invalid;
    cache->names[i] = names + offsets[i];
    if (cache->durations[i] > DURATION_MAX_US)
      cache->durations[i] = DURATION_UNKNOWN;
  }
  free(offsets);
  cache->count = count;
  cache->overhead_us = header.overhead_us <= DURATION_MAX_US ? header.overhead_us : 0;
  return 1;

invalid:
  free(offsets);
  cache_free(cache);
  return -1;
}

/* Write COUNT NAMES and DURATIONS to the cache at PATH through a temporary
   file, so that readers never see a partial cache. */
static int cache_write(const char *path, char **names, const uint64_t *durations,
                       size_t count, uint64_t overhead_us)
{
  cache_header header;
  uint32_t *offsets = xmalloc(count * sizeof *offsets);
  uint64_t names_size = 0;
  strbuf temp = { 0 };
  FILE *file;
  int ok;

  for (size_t i = 0; i < count; ++i) {
    offsets[i] = (uint32_t)names_size;
    names_size += strlen(names[i]) + 1;
  }
  if (count > CACHE_MAX_TESTS || names_size > UINT32_MAX) {
    free(offsets);
    return 0;
  }
  memset(&header, 0, sizeof header);
  memcpy(header.magic, CACHE_MAGIC, sizeof header.magic);
  header.version = CACHE_VERSION;
  header.count = (uint32_t)count;
  header.names_size = names_size;
  header.overhead_us = overhead_us;

  sb_add(&temp, path);
  sb_add(&temp, ".tmp");
  file = open_for_writing(temp.data);
  ok = file != NULL;
  if (ok) {
    ok = fwrite(&header, sizeof header, 1, file) == 1
         && fwrite(durations, sizeof *durations, count, file) == count
         && fwrite(offsets, sizeof *offsets, count, file) == count;
    for (size_t i = 0; ok && i < count; ++i) {
      size_t len = strlen(names[i]) + 1;
      ok = fwrite(names[i], 1, len, file) == len;
    }
    ok = fclose(file) == 0 && ok;
    ok = ok && replace_file(temp.data, path);
    if (!ok)
      delete_file(temp.data);
  }
  sb_free(&temp);
  free(offsets);
  return ok;
}

/* Durations recorded while running with a cache. */

static struct {
  mutex_t lock;
  int recording;
  uint64_t *durations;  /* Parallel to all_tests. */
  name_index index;     /* Over all_tests, while recording. */
  uint64_t overhead_us; /* From the cache, or measured while recording. */
  uint64_t overhead_sum;
  uint64_t overhead_samples;
} timing;

static int xml_space(char c)
{
  return is_space(c) || c == '\n';
}

/* Parse the attribute at P inside a start tag into NAME and VALUE.  Return
   the position after it, or NULL at the end of the tag. */
static const char *xml_attribute(const char *p, const char **name, size_t *name_len,
                                 const char **value, size_t *value_len)
{
  char quote;

  while (xml_space(*p))
    ++p;
  *name = p;
  while (*p && !xml_space(*p) && *p != '=' && *p != '>' && *p != '/')
    ++p;
  *name_len = (size_t)(p - *name);
  while (xml_space(*p))
    ++p;
  if (*name_len == 0 || *p != '=')
    return NULL;
  ++p;
  while (xml_space(*p))
    ++p;
  if (*p != '"' && *p != '\'')
    return NULL;
  quote = *p++;
  *value = p;
  while (*p && *p != quote)
    ++p;
  if (!*p)
    return NULL;
  *value_len = (size_t)(p - *value);
  return p + 1;
}

static void sb_add_xml(strbuf *sb, const char *text, size_t len)
{
  static const struct {
    const char *entity;
    char c;
  } entities[] = { { "&amp;", '&' }, { "&lt;", '<' }, { "&gt;", '>' },
                   { "&quot;", '"' }, { "&apos;", '\'' } };
  size_t i = 0;

  while (i < len) {
    size_t used = 0;
    if (text[i] == '&')
      for (size_t e = 0; e < sizeof entities / sizeof *entities && !used; ++e) {
        size_t n = strlen(entities[e].entity);
        if (i + n <= len && memcmp(text + i, entities[e].entity, n) == 0) {
          sb_addc(sb, entities[e].c);
          used = n;
        }
      }
    if (!used) {
      sb_addc(sb, text[i]);
      used = 1;
    }
    i += used;
  }
}

/* Store the duration of every known test case in Google Test XML.  Return
   the number of cases stored and add their durations to *TOTAL_US.  The
   caller holds timing.lock. */
static size_t record_xml_durations(const char *xml, uint64_t *total_us)
{
  strbuf name = { 0 }, full = { 0 };
  size_t found = 0;

  while ((xml = strstr(xml, "<testcase")) != NULL) {
    const char *p, *attribute, *value;
    size_t attribute_len, value_len;
    double seconds = -1;

    xml += strlen("<testcase");
    if (!xml_space(*xml))
      continue;
    sb_clear(&name);
    sb_clear(&full);
    for (p = xml; (p = xml_attribute(p, &attribute, &attribute_len, &value, &value_len))
                  != NULL;) {
      if (attribute_len == 4 && memcmp(attribute, "name", 4) == 0) {
        sb_add_xml(&name, value, value_len);
      } else if (attribute_len == 9 && memcmp(attribute, "classname", 9) == 0) {
        sb_add_xml(&full, value, value_len);
      } else if (attribute_len == 4 && memcmp(attribute, "time", 4) == 0) {
        char *end;
        seconds = strtod(value, &end);
        if (end == value)
          seconds = -1;
      }
    }
    if (name.len > 0 && full.len > 0 && seconds >= 0
        && seconds <= (double)DURATION_MAX_US / 1e6) {
      uint64_t duration = (uint64_t)(seconds * 1e6 + 0.5);
      size_t test;
      sb_addc(&full, '.');
      sb_add(&full, name.data);
      test = index_find(&timing.index, full.data);
      if (test != NOT_FOUND) {
        timing.durations[test] = duration;
        *total_us += duration;
        ++found;
      }
    }
  }
  sb_free(&name);
  sb_free(&full);
  return found;
}

/* Record the durations of CHUNK's tests from its XML file and, when every
   test reported one, the process overhead around them. */
static void record_chunk_timings(const job *chunk, const char *xml_path,
                                 uint64_t elapsed_us)
{
  char *xml = read_file(xml_path, NULL);
  uint64_t total = 0;
  size_t found;

  if (!xml)
    return;
  mutex_lock(&timing.lock);
  found = record_xml_durations(xml, &total);
  if (found == chunk->name_count) {
    timing.overhead_sum += elapsed_us > total ? elapsed_us - total : 0;
    ++timing.overhead_samples;
  }
  mutex_unlock(&timing.lock);
  free(xml);
}

/* Save the recorded timings, then report that the run has finished. */
static void finish_run(void)
{
  if (timing.recording) {
    strbuf line = { 0 };
    size_t timed = 0;

    mutex_lock(&timing.lock);
    if (timing.overhead_samples > 0)
      timing.overhead_us = timing.overhead_sum / timing.overhead_samples;
    for (size_t i = 0; i < all_tests.count; ++i)
      if (timing.durations[i] != DURATION_UNKNOWN)
        ++timed;
    if (cache_write(config.cache, all_tests.items, timing.durations, all_tests.count,
                    timing.overhead_us)) {
      sb_addf(&line, "cache-saved\t%lu\t%lu", (unsigned long)timed,
              (unsigned long)all_tests.count);
    } else {
      strbuf message = { 0 };
      sb_addf(&message, "Could not write the timing cache %s", config.cache);
      sb_add(&line, "cache-failed");
      sb_add_field(&line, message.data);
      sb_free(&message);
    }
    mutex_unlock(&timing.lock);
    emit(&line);
    sb_free(&line);
  }
  emit_simple("run-finished", NULL);
}

/* Scheduling. */

typedef struct {
  job **jobs; /* In queue order. */
  size_t count;
  uint64_t estimate_us; /* Expected time of the slowest thread, or 0. */
} chunk_plan;

typedef struct {
  uint64_t cost;
  size_t test;
} weighted_test;

static int compare_weighted(const void *a, const void *b)
{
  const weighted_test *x = a, *y = b;

  if (x->cost != y->cost)
    return x->cost > y->cost ? -1 : 1;
  return x->test < y->test ? -1 : x->test > y->test;
}

static int compare_jobs(const void *a, const void *b)
{
  const job *x = *(job *const *)a, *y = *(job *const *)b;

  if (x->expected_us != y->expected_us)
    return x->expected_us > y->expected_us ? -1 : 1;
  return strcmp(x->names[0], y->names[0]);
}

/* Assign every selected test to one of THREADS groups in GROUP_OF and store
   its expected duration in EXPECTED.  Without durations, deal the tests like
   a deck of cards.  With durations, place the longest remaining test on the
   least loaded group, so that the groups take about equally long.  Unknown
   durations count as the mean known one, and each test also carries the
   process overhead in proportion to its share of a command line of CAPACITY
   characters.  Return non-zero if durations were used. */
static int assign_groups(unsigned threads, size_t capacity, unsigned *group_of,
                         uint64_t *expected)
{
  size_t count = selected_tests.count, known = 0;
  uint64_t known_sum = 0, guess, *loads;
  size_t *sizes;
  weighted_test *order;

  for (size_t i = 0; i < count; ++i)
    if (selected_durations[i] != DURATION_UNKNOWN) {
      known_sum += selected_durations[i];
      ++known;
    }
  if (known == 0) {
    for (size_t i = 0; i < count; ++i)
      group_of[i] = (unsigned)(i % threads);
    return 0;
  }

  guess = known_sum / known;
  order = xmalloc(count * sizeof *order);
  for (size_t i = 0; i < count; ++i) {
    expected[i] = selected_durations[i] != DURATION_UNKNOWN ? selected_durations[i] : guess;
    order[i].cost = expected[i]
                    + timing.overhead_us * (strlen(selected_tests.items[i]) + 1) / capacity;
    order[i].test = i;
  }
  qsort(order, count, sizeof *order, compare_weighted);

  loads = xmalloc(threads * sizeof *loads);
  sizes = xmalloc(threads * sizeof *sizes);
  memset(loads, 0, threads * sizeof *loads);
  memset(sizes, 0, threads * sizeof *sizes);
  for (size_t k = 0; k < count; ++k) {
    unsigned best = 0;
    for (unsigned g = 1; g < threads; ++g)
      if (loads[g] < loads[best] || (loads[g] == loads[best] && sizes[g] < sizes[best]))
        best = g;
    group_of[order[k].test] = best;
    loads[best] += order[k].cost;
    ++sizes[best];
  }
  free(order);
  free(loads);
  free(sizes);
  return 1;
}

/* Split balanced groups into command lines that fit the platform limit.
   Timed chunks run longest first from the shared queue; the estimate models
   that queue rather than assuming each worker stays with its original group.
   Without timings, queue round-robin so each worker starts its own group. */
static void plan_chunks(unsigned threads, chunk_plan *plan)
{
  size_t count = selected_tests.count, rounds = 0;
  size_t base = strlen(config.exe) + 3 + strlen("--gtest_filter=") + 3
                + strlen("--gtest_output=xml:") + strlen(config.outdir) + 48;
  size_t capacity;
  unsigned *group_of = xmalloc(count * sizeof *group_of);
  uint64_t *expected = xmalloc(count * sizeof *expected);
  uint64_t *loads = xmalloc(threads * sizeof *loads);
  uint64_t *durations = xmalloc(count * sizeof *durations);
  size_t *start = xmalloc(((size_t)threads + 1) * sizeof *start);
  size_t *next = xmalloc(threads * sizeof *next);
  /* NAMES stays alive for the process lifetime; chunks borrow from it. */
  char **names = xmalloc(count * sizeof *names);
  job ***chunks = xmalloc(threads * sizeof *chunks);
  size_t *chunk_counts = xmalloc(threads * sizeof *chunk_counts);
  int timed;

  for (size_t i = 0; i < config.args.count; ++i)
    base += strlen(config.args.items[i]) + 3;
  capacity = base < COMMAND_LINE_LIMIT ? COMMAND_LINE_LIMIT - base : 1;
  timed = assign_groups(threads, capacity, group_of, expected);

  memset(start, 0, ((size_t)threads + 1) * sizeof *start);
  for (size_t i = 0; i < count; ++i)
    ++start[group_of[i] + 1];
  for (unsigned g = 0; g < threads; ++g) {
    start[g + 1] += start[g];
    next[g] = start[g];
  }
  for (size_t i = 0; i < count; ++i) {
    size_t at = next[group_of[i]]++;
    names[at] = selected_tests.items[i];
    durations[at] = timed ? expected[i] : 0;
  }

  memset(plan, 0, sizeof *plan);
  for (unsigned g = 0; g < threads; ++g) {
    size_t end = start[g + 1], from = start[g], length = 0;
    uint64_t duration = 0;

    chunks[g] = NULL;
    chunk_counts[g] = 0;
    for (size_t i = from; i <= end; ++i) {
      size_t add = i < end ? strlen(names[i]) + 1 : 0;
      if (i == end || (i > from && base + length + add > COMMAND_LINE_LIMIT)) {
        if (i > from) {
          job *chunk = new_job(JOB_CHUNK);
          chunk->names = names + from;
          chunk->name_count = i - from;
          chunk->expected_us = duration + timing.overhead_us;
          chunks[g] = xrealloc(chunks[g], (chunk_counts[g] + 1) * sizeof *chunks[g]);
          chunks[g][chunk_counts[g]++] = chunk;
          ++plan->count;
        }
        from = i;
        length = 0;
        duration = 0;
      }
      length += add;
      if (i < end)
        duration += durations[i];
    }
    if (chunk_counts[g] > rounds)
      rounds = chunk_counts[g];
  }

  plan->jobs = xmalloc(plan->count * sizeof *plan->jobs);
  plan->count = 0;
  for (size_t r = 0; r < rounds; ++r)
    for (unsigned g = 0; g < threads; ++g)
      if (r < chunk_counts[g])
        plan->jobs[plan->count++] = chunks[g][r];

  if (timed) {
    qsort(plan->jobs, plan->count, sizeof *plan->jobs, compare_jobs);
    memset(loads, 0, threads * sizeof *loads);
    for (size_t i = 0; i < plan->count; ++i) {
      unsigned best = 0;
      for (unsigned g = 1; g < threads; ++g)
        if (loads[g] < loads[best])
          best = g;
      loads[best] += plan->jobs[i]->expected_us;
      if (loads[best] > plan->estimate_us)
        plan->estimate_us = loads[best];
    }
  }

  for (unsigned g = 0; g < threads; ++g)
    free(chunks[g]);
  free(chunks);
  free(chunk_counts);
  free(group_of);
  free(expected);
  free(loads);
  free(durations);
  free(start);
  free(next);
}

static void enqueue_plan(chunk_plan *plan)
{
  mutex_lock(&queue.lock);
  queue.chunks_left = plan->count;
  for (size_t i = 0; i < plan->count; ++i)
    enqueue_locked(plan->jobs[i]);
  cond_broadcast(&queue.changed);
  mutex_unlock(&queue.lock);
  free(plan->jobs);
  plan->jobs = NULL;
}

/* Discovery. */

/* List the tests of the executable into all_tests.  Return 0 after
   reporting a failure. */
static int list_tests(void)
{
  strvec argv = { 0 };
  char *log = output_path("discover", 0, ".log");
  char *output;
  proc_result result;
  strbuf line = { 0 };

  base_argv(&argv, &config.args);
  sv_push(&argv, xstrdup("--gtest_list_tests"));
  run_process_retrying(&argv, config.cwd, log, &result);
  sv_free_items(&argv);

  if (!result.started) {
    sb_add(&line, "discover-failed");
    sb_add_field(&line, result.message);
    sb_add_field(&line, "");
    emit(&line);
  } else {
    output = read_file(log, NULL);
    if (output && result.exit_code == 0)
      parse_test_list(output, &all_tests);
    free(output);
    if (all_tests.count == 0) {
      sb_add(&line, "discover-failed");
      if (result.exit_code != 0)
        sb_addf(&line, "\tExited with status %lu", result.exit_code);
      else
        sb_add(&line, "\tNo test cases were listed");
      sb_add_field(&line, log);
      emit(&line);
    }
  }
  sb_free(&line);
  free(log);
  return all_tests.count > 0;
}

/* Fill all_tests and timing from the cache or, when rediscovering or
   without a usable cache, by listing the tests.  Return the source of the
   list, or NULL after reporting a failure. */
static const char *load_tests(void)
{
  cache_data cache = { 0 };
  int loaded = 0;

  if (config.cache) {
    loaded = cache_load(config.cache, &cache);
    if (loaded < 0) {
      strbuf line = { 0 }, message = { 0 };
      sb_addf(&message, "Ignoring invalid timing cache %s", config.cache);
      sb_add(&line, "cache-failed");
      sb_add_field(&line, message.data);
      emit(&line);
      sb_free(&line);
      sb_free(&message);
    }
  }
  if (loaded > 0 && !config.rediscover) {
    /* The names point into the cache contents, which stay loaded. */
    all_tests.items = cache.names;
    all_tests.count = all_tests.cap = cache.count;
    timing.durations = cache.durations;
    timing.overhead_us = cache.overhead_us;
    return "cache";
  }

  if (!list_tests()) {
    if (loaded > 0)
      cache_free(&cache);
    return NULL;
  }
  timing.durations = xmalloc(all_tests.count * sizeof *timing.durations);
  for (size_t i = 0; i < all_tests.count; ++i)
    timing.durations[i] = DURATION_UNKNOWN;
  if (loaded > 0) {
    /* Keep the old durations until new ones are measured. */
    name_index old;
    index_build(&old, cache.names, cache.count);
    for (size_t i = 0; i < all_tests.count; ++i) {
      size_t found = index_find(&old, all_tests.items[i]);
      if (found != NOT_FOUND)
        timing.durations[i] = cache.durations[found];
    }
    timing.overhead_us = cache.overhead_us;
    index_free(&old);
    cache_free(&cache);
  }
  return "listed";
}

static void run_discovery(void)
{
  const char *source = load_tests();
  strbuf line = { 0 };
  chunk_plan plan = { 0 };
  unsigned workers;

  if (!source)
    return;
  if (config.cache) {
    index_build(&timing.index, all_tests.items, all_tests.count);
    timing.recording = 1;
  }
  selected_durations = xmalloc(all_tests.count * sizeof *selected_durations);
  for (size_t i = 0; i < all_tests.count; ++i) {
    if (is_selected(all_tests.items[i])) {
      selected_durations[selected_tests.count] = timing.durations[i];
      sv_push(&selected_tests, all_tests.items[i]);
      sb_add(&line, "test");
      sb_add_field(&line, all_tests.items[i]);
      emit(&line);
    }
  }
  workers = selected_tests.count < config.threads ? (unsigned)selected_tests.count
                                                  : config.threads;
  if (workers > 0)
    plan_chunks(workers, &plan);
  sb_addf(&line, "discovered\t%lu\t%lu\t%u\t%s\t%llu", (unsigned long)all_tests.count,
          (unsigned long)selected_tests.count, workers, source,
          (unsigned long long)(plan.estimate_us / 1000u));
  emit(&line);
  if (workers == 0)
    finish_run();
  else
    enqueue_plan(&plan);
  sb_free(&line);
}

static void run_chunk(const job *chunk)
{
  strvec argv = { 0 };
  strbuf filter = { 0 }, line = { 0 };
  char *xml = output_path("chunk", chunk->id, ".xml");
  char *log = output_path("chunk", chunk->id, ".log");
  proc_result result;
  int last;

  base_argv(&argv, &config.args);
  sb_add(&filter, "--gtest_filter=");
  for (size_t i = 0; i < chunk->name_count; ++i) {
    if (i)
      sb_addc(&filter, ':');
    sb_add(&filter, chunk->names[i]);
  }
  sv_push(&argv, filter.data);
  sb_add(&line, "--gtest_output=xml:");
  sb_add(&line, xml);
  sv_push(&argv, xstrdup(line.data));
  sb_clear(&line);
  run_process_retrying(&argv, config.cwd, log, &result);
  sv_free_items(&argv);
  if (result.started && timing.recording)
    record_chunk_timings(chunk, xml, result.elapsed_us);

  if (result.started) {
    sb_addf(&line, "chunk-done\t%lu", result.exit_code);
    sb_add_field(&line, xml);
    sb_add_field(&line, log);
  } else {
    sb_add(&line, "chunk-failed");
    sb_add_field(&line, result.message);
  }
  for (size_t i = 0; i < chunk->name_count; ++i)
    sb_add_field(&line, chunk->names[i]);
  emit(&line);

  mutex_lock(&queue.lock);
  last = --queue.chunks_left == 0;
  mutex_unlock(&queue.lock);
  if (last)
    finish_run();
  sb_free(&line);
  free(xml);
  free(log);
}

static void run_rerun(const job *rerun)
{
  strvec argv = { 0 };
  strbuf line = { 0 };
  char *log = output_path("rerun", rerun->id, ".log");
  proc_result result;

  base_argv(&argv, &config.rerun_args);
  sb_add(&line, "--gtest_filter=");
  sb_add(&line, rerun->names[0]);
  sv_push(&argv, xstrdup(line.data));
  sb_clear(&line);
  run_process_retrying(&argv, config.cwd, log, &result);
  sv_free_items(&argv);

  if (result.started) {
    sb_add(&line, "rerun-done");
    sb_add_field(&line, rerun->rerun_id);
    sb_addf(&line, "\t%lu", result.exit_code);
    sb_add_field(&line, log);
  } else {
    sb_add(&line, "rerun-failed");
    sb_add_field(&line, rerun->rerun_id);
    sb_add_field(&line, result.message);
  }
  emit(&line);
  sb_free(&line);
  free(log);
}

NORETURN static void worker_loop(void)
{
  for (;;) {
    job *item;

    mutex_lock(&queue.lock);
    while (!queue.head)
      cond_wait(&queue.changed, &queue.lock);
    item = queue.head;
    queue.head = item->next;
    if (!queue.head)
      queue.tail = NULL;
    ++queue.active;
    mutex_unlock(&queue.lock);

    switch (item->kind) {
    case JOB_DISCOVER:
      run_discovery();
      break;
    case JOB_CHUNK:
      run_chunk(item);
      break;
    case JOB_RERUN:
      run_rerun(item);
      free(item->names[0]);
      free(item->names);
      free(item->rerun_id);
      break;
    }
    free(item);

    mutex_lock(&queue.lock);
    --queue.active;
    if (queue.quit_requested && !queue.head && queue.active == 0)
      stop_all_and_exit(0);
    mutex_unlock(&queue.lock);
  }
}

#if defined(_WIN32)
static unsigned __stdcall worker_entry(void *arg)
{
  (void)arg;
  worker_loop();
}
#else
static void *worker_entry(void *arg)
{
  (void)arg;
  worker_loop();
}
#endif

/* Commands read from Emacs. */

static void set_string(char **slot, const char *value, int path)
{
  free(*slot);
  *slot = xstrdup(value);
  if (path)
    normalize_path(*slot);
}

static void start_run(void)
{
  if (queue.started) {
    emit_error("run was already requested");
    return;
  }
  if (!config.exe || !*config.exe) {
    emit_error("run requires exe");
    return;
  }
  if (config.threads == 0)
    config.threads = 1;
  if (!config.outdir || !*config.outdir) {
    char *outdir = default_output_directory();
    if (!outdir) {
      emit_error("could not determine a temporary directory");
      return;
    }
    free(config.outdir);
    config.outdir = outdir;
  }
  if (!make_directory(config.outdir)) {
    strbuf message = { 0 };
    sb_addf(&message, "could not create output directory %s", config.outdir);
    emit_error(message.data);
    sb_free(&message);
    return;
  }
  queue.started = 1;
  for (unsigned i = 0; i < config.threads; ++i)
    if (!start_thread()) {
      if (i == 0) {
        emit_error("could not start worker threads");
        return;
      }
      break;
    }
  mutex_lock(&queue.lock);
  enqueue_locked(new_job(JOB_DISCOVER));
  cond_broadcast(&queue.changed);
  mutex_unlock(&queue.lock);
}

static void handle_command(char **fields, size_t count)
{
  const char *command = fields[0];
  int configuring = strcmp(command, "exe") == 0 || strcmp(command, "cwd") == 0
                    || strcmp(command, "outdir") == 0 || strcmp(command, "arg") == 0
                    || strcmp(command, "rerun-arg") == 0 || strcmp(command, "threads") == 0
                    || strcmp(command, "filter") == 0
                    || strcmp(command, "exclude-slow") == 0
                    || strcmp(command, "cache") == 0
                    || strcmp(command, "rediscover") == 0;
  strbuf message = { 0 };

  if (configuring && queue.started) {
    sb_addf(&message, "%s cannot change after run", command);
    emit_error(message.data);
  } else if (strcmp(command, "exclude-slow") == 0 && count == 1) {
    config.exclude_slow = 1;
  } else if (strcmp(command, "rediscover") == 0 && count == 1) {
    config.rediscover = 1;
  } else if (configuring && count == 2) {
    const char *value = fields[1];
    if (strcmp(command, "exe") == 0) {
      set_string(&config.exe, value, 1);
    } else if (strcmp(command, "cwd") == 0) {
      set_string(&config.cwd, value, 1);
    } else if (strcmp(command, "outdir") == 0) {
      set_string(&config.outdir, value, 1);
    } else if (strcmp(command, "cache") == 0) {
      set_string(&config.cache, value, 1);
    } else if (strcmp(command, "filter") == 0) {
      set_string(&config.filter, value, 0);
    } else if (strcmp(command, "arg") == 0) {
      sv_push(&config.args, xstrdup(value));
    } else if (strcmp(command, "rerun-arg") == 0) {
      sv_push(&config.rerun_args, xstrdup(value));
    } else {
      char *end;
      unsigned long threads = strtoul(value, &end, 10);
      if (*value && !*end && threads >= 1 && threads <= MAX_THREADS) {
        config.threads = (unsigned)threads;
      } else {
        sb_addf(&message, "threads must be between 1 and %u", MAX_THREADS);
        emit_error(message.data);
      }
    }
  } else if (strcmp(command, "run") == 0 && count == 1) {
    start_run();
  } else if (strcmp(command, "rerun") == 0 && count == 3) {
    if (!queue.started) {
      emit_error("rerun requires run");
    } else {
      job *rerun = new_job(JOB_RERUN);
      rerun->rerun_id = xstrdup(fields[1]);
      rerun->names = xmalloc(sizeof *rerun->names);
      rerun->names[0] = xstrdup(fields[2]);
      rerun->name_count = 1;
      mutex_lock(&queue.lock);
      enqueue_locked(rerun);
      cond_broadcast(&queue.changed);
      mutex_unlock(&queue.lock);
    }
  } else if (strcmp(command, "quit") == 0 && count == 1) {
    mutex_lock(&queue.lock);
    queue.quit_requested = 1;
    if (!queue.head && queue.active == 0)
      stop_all_and_exit(0);
    mutex_unlock(&queue.lock);
  } else if (strcmp(command, "stop") == 0 && count == 1) {
    stop_all_and_exit(0);
  } else {
    sb_addf(&message, "invalid command: %s with %lu argument(s)", command,
            (unsigned long)(count - 1));
    emit_error(message.data);
  }
  sb_free(&message);
}

static int read_line(strbuf *line)
{
  int c;

  sb_clear(line);
  while ((c = getc(stdin)) != EOF && c != '\n')
    sb_addc(line, (char)c);
  if (c == EOF && line->len == 0)
    return 0;
  while (line->len > 0 && line->data[line->len - 1] == '\r')
    line->data[--line->len] = '\0';
  return 1;
}

static void usage(FILE *stream)
{
  fputs("Usage: " RUNNER_NAME " [--version | --help]\n"
        "\n"
        "Runs Google Test cases in parallel for Emacs.  Without arguments it\n"
        "reads tab-separated commands from stdin and writes events to stdout.\n"
        "See README.md in the Emacs configuration for the protocol.\n",
        stream);
}

int main(int argc, char **argv)
{
  strbuf line = { 0 };
  strvec fields = { 0 };

  if (argc > 1) {
    if (argc == 2 && strcmp(argv[1], "--version") == 0) {
      puts(RUNNER_NAME " " RUNNER_VERSION " (protocol " PROTOCOL_VERSION ")");
      return 0;
    }
    if (argc == 2 && strcmp(argv[1], "--help") == 0) {
      usage(stdout);
      return 0;
    }
    usage(stderr);
    return 2;
  }

  platform_init();
  mutex_init(&output_lock);
  mutex_init(&children.lock);
  cond_init(&children.exited);
  mutex_init(&queue.lock);
  cond_init(&queue.changed);
  mutex_init(&timing.lock);
  config.threads = 1;

  sb_add(&line, "hello\t" RUNNER_NAME "\t" PROTOCOL_VERSION);
  emit(&line);

  while (read_line(&line)) {
    char *field = line.data;
    if (line.len == 0)
      continue;
    fields.count = 0;
    for (;;) {
      char *tab = strchr(field, '\t');
      sv_push(&fields, field);
      if (!tab)
        break;
      *tab = '\0';
      field = tab + 1;
    }
    handle_command(fields.items, fields.count);
  }
  /* Emacs closed the pipe or exited: nothing is left to report to. */
  stop_all_and_exit(0);
}
