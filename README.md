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
  thread count (default: half the logical CPUs), `f` sets a name filter, and
  `u`/`i` run unit/integration tests. Outside TnT it prompts for the Google
  Test executable. Failed tests are rerun with logging; press `TAB` on a
  failure to expand its log. Killing the result buffer stops the batch.

## emacs-test-runner

Emacs used to start one process, with its own pipes, per test thread. On
Windows Emacs cannot create many process pipes, so large thread counts left
tests unrun with `Creating pipe: Too many open files`. `emacs-test-runner`
needs a single pipe to Emacs instead:

- It runs `--gtest_list_tests`, applies the batch filters and splits the
  selected tests across the requested number of worker threads, keeping each
  command line within the platform limit.
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
line breaks. Protocol version: 1.

Commands:

| Command | Meaning |
| --- | --- |
| `exe PATH` | Google Test executable (required). |
| `cwd DIR` | Working directory for test processes. |
| `outdir DIR` | Directory for logs and XML (default: a new directory under the temp directory). |
| `arg ARG` | Argument for discovery and test runs; repeatable. |
| `rerun-arg ARG` | Argument for reruns; repeatable. |
| `threads N` | Parallel test processes, 1-1024 (default 1). |
| `filter TEXT` | Run only tests whose full name contains `TEXT`. |
| `exclude-slow` | Skip tests with `SLOW` at the start of the suite, the case or a `/` segment. |
| `run` | Discover and run the tests. Configuration is fixed afterwards. |
| `rerun ID NAME` | Run test `NAME` alone with the rerun arguments. |
| `quit` | Exit once queued work is done. |
| `stop` | Kill all test processes and exit immediately. Closing stdin does the same. |

Events:

| Event | Meaning |
| --- | --- |
| `hello emacs-test-runner VERSION` | Sent at startup. |
| `test NAME` | One per selected test, before `discovered`. |
| `discovered TOTAL SELECTED THREADS` | Discovery finished; `THREADS` processes will run in parallel. |
| `chunk-done EXIT XML LOG NAME...` | A test process for `NAME...` exited. |
| `chunk-failed MESSAGE NAME...` | A test process could not start. |
| `run-finished` | All selected tests have been reported. |
| `rerun-done ID EXIT LOG` | Rerun `ID` exited. |
| `rerun-failed ID MESSAGE` | Rerun `ID` could not start. |
| `discover-failed MESSAGE LOG` | Discovery failed; `LOG` may be empty. |
| `error MESSAGE` | Invalid command or configuration. |

## Help

### This file is not loaded

If Windows does not use the default `.emacs.d` folder, find the init file
that was used with `M-x describe-variable RET user-init-file`, then load this
configuration from there with `(load "~/.emacs.d/init.el")`.

### lsp-bridge does not work

Search Windows settings for `App Execution Aliases` and turn off python.
