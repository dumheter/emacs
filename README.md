# Emacs configuration

Personal Emacs 30 configuration for Windows and Linux. `init.el` holds the
configuration, `local-packages/` holds local libraries, and
`emacs-test-runner/` holds a small C program that runs Google Test batches.

## Quick start

1. Clone this repository as your Emacs directory: `~/.emacs.d` on Linux,
   `%APPDATA%\.emacs.d` on Windows.
2. Install [lsp-bridge](#lsp-bridge) into `~/lsp-bridge`.
3. [Build emacs-test-runner](#build-emacs-test-runner) for parallel test
   batches.
4. On Windows, do the [Windows setup](#windows-setup).
5. Start Emacs. The first start refreshes the package archives and installs
   the configured packages, so it takes a while.

### lsp-bridge

```sh
cd ~ && mkdir lsp-bridge && cd lsp-bridge
git init
git remote add origin https://github.com/manateelazycat/lsp-bridge.git   # or git@github.com:manateelazycat/lsp-bridge.git
git pull origin master
```

#### clangd on big projects

1. Locate your lsp-bridge install.
2. In the `langserver` folder, open `clangd.json`.
3. Append these arguments:

   ```json
   "--completion-style=detailed",
   "--background-index=false"
   ```

### Build emacs-test-runner

The batch test command (`C-c p c b`) needs `emacs-test-runner`. Emacs starts
it only while a batch runs and looks for it at
`emacs-test-runner/build/emacs-test-runner[.exe]` (see
`my-projectile-tests-runner-program`).

Requirements: CMake 3.16 or newer and a C11 compiler (clang, MSVC or gcc).
The build is a release build (`-O3`/`/O2`) with all warnings enabled and
treated as errors. From this directory:

Windows with Ninja and clang:

```powershell
cmake -S emacs-test-runner -B emacs-test-runner/build -G Ninja -DCMAKE_C_COMPILER=clang
cmake --build emacs-test-runner/build
```

Windows with Visual Studio (from any shell; the executable still lands in
`emacs-test-runner/build/`):

```powershell
cmake -S emacs-test-runner -B emacs-test-runner/build
cmake --build emacs-test-runner/build --config Release
```

Linux:

```sh
cmake -S emacs-test-runner -B emacs-test-runner/build
cmake --build emacs-test-runner/build
```

Check the result with `emacs-test-runner/build/emacs-test-runner --version`.
Rebuild after pulling changes to `emacs-test-runner/`; Emacs reports a
protocol mismatch if the build is outdated.

With Python 3 available, check cache refresh, runs without cache, filtering,
repeats and multi-process scheduling on Windows:

```powershell
python emacs-test-runner\tests.py emacs-test-runner\build\emacs-test-runner.exe
```

Use `/` paths and omit `.exe` on Linux. These checks use a fake Google Test
executable and do not run project tests.

### Windows setup

#### Open files in the same window

Associate your files with `emacsclientw.exe`. This opens a new window; to
reuse the current one, open regedit at
`HKEY_CLASSES_ROOT\Applications\emacsclientw.exe\shell\open\command` and make
sure it includes `--no-wait`:

```
"C:\Path\To\emacsclientw.exe" --no-wait "%1"
```

#### Hunspell

```powershell
choco install hunspell.portable
```

Copy `en_US.aff` and `en_US.dic` to `C:/Hunspell`.

## Usage notes

### C and C++

Pause on a C or C++ type name for clangd hover (including size when
available). `C-g` hides the automatic hover until point moves or the buffer
changes; symbol highlights remain visible. Use `C-c l h` to request hover
immediately. C and C++ buffers also highlight symbol references and show
breadcrumbs.

### Projectile tests

- `C-c p c u` runs unit tests and `C-c p c n` runs integration tests. Both
  prompt for a command, prefilled with the matching executable in TnT.
- `C-c p c b` opens the batch settings: `s` excludes SLOW tests, `t` sets the
  thread count (default: half the logical CPUs), `f` sets a name filter, `d`
  toggles discovery mode, `r` toggles **Run without cache**, `l`/`c` toggle
  `disableLogs`/`disableCallstackResolution`, `g` sets **Google Test repeat**,
  `p` sets **Parallel repeat**, and `u`/`i` run
  unit/integration tests. Both disable flags default to enabled, apply only
  to TnT batches, and are saved across Emacs sessions with the other settings.
  Outside TnT it prompts for the Google Test executable. Failed tests are
  rerun with logging; press `TAB` on a failure to expand its log. Killing the result
  buffer stops the batch.
- Settings values are muted at their defaults; active overrides use the
  theme's warning color. An explicit thread count equal to the automatic
  default stays muted, as does discovery mode when **Run without cache**
  overrides it. The TnT disable flags are muted when enabled (their default)
  and highlighted when disabled.
- **Google Test repeat** passes `--gtest_repeat=N` to each test process and
  diagnostic rerun, repeating its selected cases sequentially inside that
  process. **Parallel repeat** schedules N independent iterations of every
  matching test across the worker threads. Both default to 1, accept integers
  from 1 to 1,000,000, and persist with the other settings; infinite native
  repeats (`-1`) are not supported. The counts multiply: setting Google Test
  repeat to 5 and parallel repeat to 20 runs each selected case 100 times.
  To stress one case across threads, narrow the name filter to that case and
  increase parallel repeat.
- Each parallel iteration has its own result entry and original failure log.
  Later passes and diagnostic reruns never replace a failed iteration's result.
  Google Test XML covers only the final native repeat; if a repeated process
  exits unsuccessfully despite passing final XML, its affected cases are
  marked **FAILED PROCESS** instead of reported as passing. The original log
  remains available alongside the diagnostic rerun.
- Listing the tests is slow, so batches normally reuse the cached test list
  and split the tests using their recorded durations. Every run refreshes the
  durations and process overhead, so balancing adapts without rediscovery.
  Discovery mode also lists the tests again; use it after adding or removing
  tests. The first batch for an executable always discovers. Caches live in
  `.cache/emacs-test-runner/`.
- **Run without cache** always discovers tests and distributes the selected tests
  round-robin, ignoring cached test lists, durations and process overhead.
  It does not read, create or update the timing cache or record timings.
  It overrides discovery mode while enabled; turning it off restores the
  saved discovery setting. Filters, SLOW exclusion and thread count still apply.
- More threads are not necessarily faster: test executables can have their
  own worker threads, and concurrent process startup also competes for
  resources. Compare nearby thread counts with `t`, keeping the same filter
  and SLOW setting. The estimate includes test-body time and process overhead;
  faster cache loading cannot remove that overhead.

## emacs-test-runner

Emacs used to start one process, with its own pipes, per test thread. On
Windows Emacs cannot create many process pipes, so large thread counts left
tests unrun with `Creating pipe: Too many open files`. `emacs-test-runner`
needs a single pipe to Emacs instead:

- It runs `--gtest_list_tests`, applies the batch filters and splits the
  selected tests across the requested number of worker threads, keeping each
  command line within the platform limit.
- With a timing cache, it reads the test list and durations from the cache
  instead of listing the tests. It then assigns the longest remaining test
  to the least loaded thread until all tests are placed, so the threads
  finish together. Tests without a recorded duration count as the mean
  duration, and the measured per-process overhead counts too. Without
  durations, it deals the tests round-robin.
- If command-line limits split a group into several processes, the shared
  queue starts the longest estimated chunks first. The estimate models that
  queue, including startup overhead for every chunk, rather than assuming
  workers stay assigned to their original groups.
- Parallel repeats copy the planned chunks into independent iteration jobs
  in the shared queue, so even a single selected case can occupy multiple
  workers. Each iteration uses distinct XML and log files. Native repeats
  run inside each process and do not apply to test discovery.
- On every run with a cache, it reads each test's `time` from the Google Test
  XML. It also measures the per-process overhead (process wall time minus
  test time) and saves the list and durations to the cache after the run.
  Durations of tests that did not run this time are kept from the previous
  cache. Cached runs do not list the tests again.
  With native repeats, only the final iteration's XML timings are available:
  those refresh the per-test durations, but the cached process overhead is
  preserved rather than counting earlier iterations as startup overhead.
  Estimates account for both repeat counts.
- Each worker runs one test process at a time with its output redirected to a
  file in a temporary directory created by Emacs (`emacs-test-runner-*` under
  `TEMP`). Emacs reads the Google Test XML and logs from there and deletes the
  directory when the result buffer is killed or reused.
- If a process cannot start for lack of system resources while others run,
  the worker waits for one to exit and retries.
- Test processes die with the runner: on Windows they run in a kill-on-close
  job object; on Linux they get `SIGKILL` when the runner dies.

### Protocol

Emacs writes commands to the runner's stdin and reads events from its stdout.
Both are UTF-8 lines of tab-separated fields, so fields cannot contain tabs or
line breaks. Protocol version: 4.

Commands:

| Command | Meaning |
| --- | --- |
| `exe PATH` | Google Test executable (required). |
| `cwd DIR` | Working directory for test processes. |
| `outdir DIR` | Directory for logs and XML (default: a new directory under the temp directory). |
| `arg ARG` | Argument for discovery and test runs; repeatable. |
| `rerun-arg ARG` | Argument for reruns; repeatable. |
| `threads N` | Parallel test processes, 1-1024 (default 1). |
| `gtest-repeat N` | Sequential `--gtest_repeat=N` within each test process and diagnostic rerun, 1-1000000 (default 1); not used for discovery. |
| `repeat N` | Independent parallel iterations of each selected test, 1-1000000 (default 1). |
| `filter TEXT` | Run only tests whose full name contains `TEXT`. |
| `exclude-slow` | Skip tests with `SLOW` at the start of the suite, the case or a `/` segment. |
| `cache PATH` | Timing cache file. Without `rediscover`, a readable cache replaces discovery. Omit to always discover, schedule round-robin and disable timing recording and cache reads/writes. |
| `rediscover` | List the tests even if the cache exists. All cached runs record timings. |
| `run` | Discover and run the tests. Configuration is fixed afterwards. |
| `rerun ID NAME` | Run test `NAME` alone with the rerun arguments. |
| `quit` | Exit once queued work is done. |
| `stop` | Kill all test processes and exit immediately. Closing stdin does the same. |

Events:

| Event | Meaning |
| --- | --- |
| `hello emacs-test-runner VERSION` | Sent at startup. |
| `test NAME ITERATION` | One per selected test and parallel iteration (1-based), before `discovered`. |
| `discovered TOTAL SELECTED THREADS SOURCE ESTIMATE` | Tests are known; `TOTAL` is the unique discovered case count and `SELECTED` includes parallel iterations. `THREADS` processes will run in parallel. `SOURCE` is `listed` or `cache`; `ESTIMATE` is the expected run time in milliseconds, or 0 without timings. |
| `chunk-done ITERATION EXIT XML LOG NAME...` | A test process for `NAME...` in parallel `ITERATION` exited. |
| `chunk-failed ITERATION MESSAGE NAME...` | A test process in parallel `ITERATION` could not start. |
| `cache-saved TIMED TOTAL` | Saved the cache, with durations for `TIMED` of `TOTAL` tests. Sent before `run-finished` on both discovery and cached runs. |
| `cache-failed MESSAGE` | The cache was invalid (and is rebuilt) or could not be written. The batch continues. |
| `run-finished` | All selected tests have been reported. |
| `rerun-done ID EXIT LOG` | Rerun `ID` exited. |
| `rerun-failed ID MESSAGE` | Rerun `ID` could not start. |
| `discover-failed MESSAGE LOG` | Discovery failed; `LOG` may be empty. |
| `error MESSAGE` | Invalid command or configuration. |

### Timing cache format

A cache stores the full test list (not only the selected tests). Each array
loads with a single `memcpy`. All integers use the native byte order:

| Offset | Content |
| --- | --- |
| 0 | Magic `ETRCACHE` (8 bytes). |
| 8 | `uint32` format version (1). |
| 12 | `uint32` test count `N`. |
| 16 | `uint64` size `S` of the name block. |
| 24 | `uint64` mean per-process overhead in microseconds. |
| 32 | `N` × `uint64` test durations in microseconds; `UINT64_MAX` if unknown. |
| 32 + 8N | `N` × `uint32` offsets of the names in the name block. |
| 32 + 12N | `S` bytes of NUL-terminated full test names (`Suite.Case`). |

The runner writes a temporary file and renames it over the cache. A cache
with the wrong size, magic, version or offsets is reported, ignored and
rebuilt by discovery.

## Help

### This file is not loaded

If Windows does not use the default `.emacs.d` folder, find the init file
that was used with `M-x describe-variable RET user-init-file`, then load this
configuration from there with `(load "~/.emacs.d/init.el")`.

### lsp-bridge does not work

Search Windows settings for `App Execution Aliases` and turn off python.
