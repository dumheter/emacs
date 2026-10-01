/*
 * emacs-test-runner - run Google Test cases in parallel on behalf of Emacs.
 *
 * Emacs starts one runner per batch and talks to it over stdin/stdout with a
 * line-based protocol described in ../README.md.  The runner discovers the
 * test cases, runs them from worker threads that each own at most one child
 * process, and redirects every child's output to a file in the output
 * directory.  Emacs therefore needs a single pipe however many tests run in
 * parallel.  Child processes are killed when the runner stops or dies.
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
#include <unistd.h>
#if defined(__linux__)
#include <sys/prctl.h>
#endif
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RUNNER_NAME "emacs-test-runner"
#define RUNNER_VERSION "1.0"
#define PROTOCOL_VERSION "1"
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

  _setmode(_fileno(stdin), _O_BINARY);
  _setmode(_fileno(stdout), _O_BINARY);

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

static char *read_file(const char *path)
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

    mutex_lock(&children.lock);
    exits = children.exits;
    mutex_unlock(&children.lock);
    run_process(argv, cwd, log_path, result);
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
  strvec args;
  strvec rerun_args;
  unsigned threads;
  int exclude_slow;
} config;

/* Work queue shared by the worker threads. */

typedef enum { JOB_DISCOVER, JOB_CHUNK, JOB_RERUN } job_kind;

typedef struct job {
  job_kind kind;
  unsigned id;
  char *rerun_id;
  char **names; /* Borrowed from selected_tests, except for reruns. */
  size_t name_count;
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

static strvec selected_tests;

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

/* Split the selected tests across THREADS groups like a dealt deck of cards,
   then cut each group into command lines that fit the platform limit.  The
   chunks are queued round-robin, so every thread starts on its own group. */
static void queue_chunks(unsigned threads)
{
  size_t base = strlen(config.exe) + 3 + strlen("--gtest_filter=") + 3
                + strlen("--gtest_output=xml:") + strlen(config.outdir) + 48;
  size_t rounds = 0, total = 0;
  job ***chunks = xmalloc(threads * sizeof *chunks);
  size_t *chunk_counts = xmalloc(threads * sizeof *chunk_counts);

  for (size_t i = 0; i < config.args.count; ++i)
    base += strlen(config.args.items[i]) + 3;

  for (unsigned g = 0; g < threads; ++g) {
    /* NAMES stays alive for the process lifetime; chunks borrow from it. */
    char **names = xmalloc((selected_tests.count / threads + 1) * sizeof *names);
    size_t count = 0, start = 0, length = 0;

    chunks[g] = NULL;
    chunk_counts[g] = 0;
    for (size_t i = g; i < selected_tests.count; i += threads)
      names[count++] = selected_tests.items[i];
    for (size_t i = 0; i <= count; ++i) {
      size_t add = i < count ? strlen(names[i]) + 1 : 0;
      if (i == count || (i > start && base + length + add > COMMAND_LINE_LIMIT)) {
        if (i > start) {
          job *chunk = new_job(JOB_CHUNK);
          chunk->names = names + start;
          chunk->name_count = i - start;
          chunks[g] = xrealloc(chunks[g], (chunk_counts[g] + 1) * sizeof *chunks[g]);
          chunks[g][chunk_counts[g]++] = chunk;
          ++total;
        }
        start = i;
        length = 0;
      }
      length += add;
    }
    if (chunk_counts[g] > rounds)
      rounds = chunk_counts[g];
    if (count == 0)
      free(names);
  }

  mutex_lock(&queue.lock);
  queue.chunks_left = total;
  for (size_t r = 0; r < rounds; ++r)
    for (unsigned g = 0; g < threads; ++g)
      if (r < chunk_counts[g])
        enqueue_locked(chunks[g][r]);
  cond_broadcast(&queue.changed);
  mutex_unlock(&queue.lock);

  for (unsigned g = 0; g < threads; ++g)
    free(chunks[g]);
  free(chunks);
  free(chunk_counts);
}

static void run_discovery(void)
{
  strvec argv = { 0 };
  strvec discovered = { 0 };
  char *log = output_path("discover", 0, ".log");
  char *output;
  proc_result result;
  strbuf line = { 0 };
  unsigned workers;

  base_argv(&argv, &config.args);
  sv_push(&argv, xstrdup("--gtest_list_tests"));
  run_process_retrying(&argv, config.cwd, log, &result);
  sv_free_items(&argv);

  if (!result.started) {
    sb_add(&line, "discover-failed");
    sb_add_field(&line, result.message);
    sb_add_field(&line, "");
    emit(&line);
    goto done;
  }
  output = read_file(log);
  if (output && result.exit_code == 0)
    parse_test_list(output, &discovered);
  free(output);
  if (discovered.count == 0) {
    sb_add(&line, "discover-failed");
    if (result.exit_code != 0)
      sb_addf(&line, "\tExited with status %lu", result.exit_code);
    else
      sb_add(&line, "\tNo test cases were listed");
    sb_add_field(&line, log);
    emit(&line);
    goto done;
  }

  for (size_t i = 0; i < discovered.count; ++i) {
    if (is_selected(discovered.items[i])) {
      sv_push(&selected_tests, discovered.items[i]);
      sb_add(&line, "test");
      sb_add_field(&line, discovered.items[i]);
      emit(&line);
    } else {
      free(discovered.items[i]);
    }
  }
  workers = selected_tests.count < config.threads ? (unsigned)selected_tests.count
                                                  : config.threads;
  sb_addf(&line, "discovered\t%lu\t%lu\t%u", (unsigned long)discovered.count,
          (unsigned long)selected_tests.count, workers);
  emit(&line);
  if (workers == 0)
    emit_simple("run-finished", NULL);
  else
    queue_chunks(workers);

done:
  /* Selected names now belong to selected_tests. */
  free(discovered.items);
  sb_free(&line);
  free(log);
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
    emit_simple("run-finished", NULL);
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
                    || strcmp(command, "exclude-slow") == 0;
  strbuf message = { 0 };

  if (configuring && queue.started) {
    sb_addf(&message, "%s cannot change after run", command);
    emit_error(message.data);
  } else if (strcmp(command, "exclude-slow") == 0 && count == 1) {
    config.exclude_slow = 1;
  } else if (configuring && count == 2) {
    const char *value = fields[1];
    if (strcmp(command, "exe") == 0) {
      set_string(&config.exe, value, 1);
    } else if (strcmp(command, "cwd") == 0) {
      set_string(&config.cwd, value, 1);
    } else if (strcmp(command, "outdir") == 0) {
      set_string(&config.outdir, value, 1);
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
