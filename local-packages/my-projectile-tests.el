;;; my-projectile-tests.el --- Projectile test commands and parallel gtest runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run unit or integration tests directly, or discover and run their Google
;; Test cases in parallel.  A settings buffer launches the batch and the
;; result buffer folds each failed case's verbose rerun output under its own
;; heading.  The final summary includes the total elapsed time.
;;
;; Batches are run by the external emacs-test-runner program (see README.md),
;; which Emacs starts per batch and drives over a single pipe.  The runner
;; discovers the cases, runs them in parallel and writes each process's
;; output to files that Emacs reads, so the thread count is not limited by
;; the number of process pipes Emacs can create.
;;
;; Listing the tests is slow, so the runner keeps the test list and each
;; test's duration in a timing cache per executable.  Batches normally use
;; the cache and schedule the tests so that all threads finish together.
;; Every cached run refreshes the durations.  Discovery mode (d in the
;; settings) also lists the tests again.  Run without cache (r) discovers and
;; distributes tests round-robin without reading or writing a cache.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'projectile)
(require 'subr-x)
(require 'xml)

(defconst my-projectile-tests--tnt-executables
  '((unit . "Extension.BattlefieldOnline.Runtime.Test_Win64_release_Dll.exe")
    (integration . "Extension.BattlefieldOnline.Runtime.IntegrationTest_Win64_release_Dll.exe")))

(defvar my-projectile-test-project-unit-cmd-map (make-hash-table :test 'equal)
  "Last unit test command used in each project compilation directory.")

(defvar my-projectile-test-project-integration-cmd-map (make-hash-table :test 'equal)
  "Last integration test command used in each project compilation directory.")

(defvar my-projectile-tests-batch-settings nil
  "Saved batch settings: :exclude-slow, :threads, :filter, :discover and :fresh.
The thread count defaults to half the available logical CPUs.")

(defvar my-projectile-tests-cache-directory
  (locate-user-emacs-file ".cache/emacs-test-runner/")
  "Directory for the emacs-test-runner timing caches, one per executable.")

(defvar my-projectile-tests-runner-program
  (expand-file-name (concat "emacs-test-runner/build/emacs-test-runner"
                            (if (eq system-type 'windows-nt) ".exe" ""))
                    user-emacs-directory)
  "The emacs-test-runner executable used for batch tests.
Build it as described in README.md.")

(defconst my-projectile-tests--runner-protocol "3"
  "Protocol version this library expects from emacs-test-runner.")

(defconst my-projectile-tests--max-threads 1024
  "Largest thread count emacs-test-runner accepts.")

(defconst my-projectile-tests--log-excerpt-size 20000
  "Number of trailing log characters shown for a batch error.")

(defun my-projectile-tests--tnt-p (root)
  "Return non-nil if ROOT is a TnT project root."
  (string= (file-name-nondirectory (directory-file-name root)) "TnT"))

(defun my-projectile-tests--cache-file (executable)
  "Return the timing cache file for the Google Test EXECUTABLE."
  (let ((path (expand-file-name executable)))
    (expand-file-name
     (format "%s-%s.etr" (file-name-nondirectory path)
             (substring (md5 (if (memq system-type '(windows-nt ms-dos))
                                 (downcase path)
                               path))
                        0 8))
     my-projectile-tests-cache-directory)))

(defun my-projectile-test-project--run (kind command-map arg)
  "Run KIND tests using COMMAND-MAP to remember the command.
Prefix ARG forces a command prompt."
  (let* ((root (projectile-acquire-root))
         (directory (projectile-compilation-dir))
         (command (or (gethash directory command-map)
                      (when (my-projectile-tests--tnt-p root)
                        (concat "Local\\Bin\\Win64-Dll\\release\\"
                                (alist-get kind my-projectile-tests--tnt-executables)
                                " -disableLogs -disableCallstackResolution --gtest_filter=*")))))
    (projectile--run-project-cmd command
                                 (when (projectile--cache-project-commands-p)
                                   command-map)
                                 :command-type kind
                                 :directory directory
                                 :show-prompt (or arg (null command))
                                 :prompt-prefix (format "%s test command: " (capitalize (symbol-name kind)))
                                 :save-buffers t
                                 :use-comint-mode (projectile-use-comint-mode-p 'test))))

(defun my-projectile-test-project-unit (arg)
  "Run unit tests for the current project.
Prompt for the command, defaulting to the TnT unit test executable.
With prefix ARG, force the command prompt."
  (interactive "P")
  (my-projectile-test-project--run
   'unit my-projectile-test-project-unit-cmd-map arg))

(defun my-projectile-test-project-integration (arg)
  "Run integration tests for the current project.
Prompt for the command, defaulting to the TnT integration test executable.
With prefix ARG, force the command prompt."
  (interactive "P")
  (my-projectile-test-project--run
   'integration my-projectile-test-project-integration-cmd-map arg))

(cl-defstruct my-projectile-tests--batch
  kind root executable flags buffer cpus thread-limit threads exclude-slow filter
  discover cache source cache-time estimate timed
  process pending outdir discovering tests results logs errors tests-done
  rerun-names reruns-total reruns-done start-time elapsed finished cancelled)

(defvar-local my-projectile-tests--current-batch nil)
(defvar-local my-projectile-tests--project-root nil)

(define-derived-mode my-projectile-tests-settings-mode special-mode "Projectile Batch Settings"
  "Major mode for choosing and launching parallel Google Test batches.
Press s to exclude SLOW tests, t to set threads, f to set an include filter,
d to toggle discovery mode, r to toggle runs without cache, or u/i to
run unit/integration tests with the displayed settings.")

(defun my-projectile-tests--default-threads ()
  "Return the default number of parallel test threads."
  (max 1 (/ (1+ (num-processors)) 2)))

(defun my-projectile-tests--insert-setting (key label value value-face)
  "Insert a settings row with KEY, LABEL, VALUE and VALUE-FACE."
  (insert "  " (propertize (format "[%s]" key)
                          'face '(:inherit font-lock-keyword-face :weight bold))
          "  " (format "%-29s" label)
          (propertize value 'face value-face) "\n"))

(defun my-projectile-tests--render-settings ()
  "Show the current batch settings in the settings buffer."
  (let* ((inhibit-read-only t)
         (settings my-projectile-tests-batch-settings)
         (exclude-slow (plist-get settings :exclude-slow))
         (threads (plist-get settings :threads))
         (filter (plist-get settings :filter))
         (discover (plist-get settings :discover))
         (fresh (plist-get settings :fresh))
         (thread-count (or threads (my-projectile-tests--default-threads))))
    (erase-buffer)
    (insert "  "
            (propertize "PROJECTILE  /  TEST RUNNER"
                        'face '(:inherit font-lock-function-name-face
                                         :height 1.5 :weight bold))
            "\n  "
            (propertize my-projectile-tests--project-root 'face 'shadow)
            "\n\n  "
            (propertize "SETTINGS" 'face '(:inherit font-lock-keyword-face
                                                   :weight bold))
            "\n")
    (my-projectile-tests--insert-setting
     "s" "SLOW tests:"
     (if exclude-slow "EXCLUDE" "INCLUDE")
     (if exclude-slow 'success 'shadow))
    (my-projectile-tests--insert-setting
     "t" "Threads:"
     (if threads (number-to-string threads)
       (format "auto (%d)" thread-count))
     'font-lock-constant-face)
    (my-projectile-tests--insert-setting
     "f" "Include tests containing:"
     (if (or (null filter) (string-empty-p filter)) "all" filter)
     (if (or (null filter) (string-empty-p filter))
         'shadow 'font-lock-string-face))
    (my-projectile-tests--insert-setting
     "d" "Discovery mode:"
     (cond (fresh "IGNORED (run without cache is ON)")
           (discover "ON (list tests and record timings)")
           (t "OFF (use cached tests and timings)"))
     (if (and discover (not fresh)) 'success 'shadow))
    (my-projectile-tests--insert-setting
     "r" "Run without cache:"
     (if fresh "ON (discover; no cache or timings)" "OFF")
     (if fresh 'success 'shadow))
    (insert "\n  "
            (propertize "RUN" 'face '(:inherit font-lock-keyword-face
                                              :weight bold))
            "\n  "
            (propertize "[u]  UNIT TESTS" 'face '(:inherit success :weight bold))
            "        "
            (propertize "[i]  INTEGRATION TESTS"
                        'face '(:inherit success :weight bold))
            "\n")
    (insert "\n  "
            (propertize "[q]  Close settings" 'face 'shadow)
            "\n")
    (goto-char (point-min))))

(defun my-projectile-tests--toggle-slow ()
  "Toggle exclusion of tests whose suite or case starts with SLOW."
  (interactive)
  (setq my-projectile-tests-batch-settings
        (plist-put my-projectile-tests-batch-settings :exclude-slow
                   (not (plist-get my-projectile-tests-batch-settings :exclude-slow))))
  (my-projectile-tests--render-settings))

(defun my-projectile-tests--toggle-discovery ()
  "Toggle discovery mode.
In discovery mode a batch lists the test cases again and records how
long each one takes.  Otherwise it uses the list and timings recorded
by the last discovery to give every thread an equal share of work.
Run without cache overrides this setting without changing its saved value."
  (interactive)
  (setq my-projectile-tests-batch-settings
        (plist-put my-projectile-tests-batch-settings :discover
                   (not (plist-get my-projectile-tests-batch-settings :discover))))
  (my-projectile-tests--render-settings))

(defun my-projectile-tests--toggle-fresh ()
  "Toggle runs without reading or writing the timing cache.
Runs without cache always discover tests and distribute them round-robin.
This overrides discovery mode while enabled."
  (interactive)
  (setq my-projectile-tests-batch-settings
        (plist-put my-projectile-tests-batch-settings :fresh
                   (not (plist-get my-projectile-tests-batch-settings :fresh))))
  (my-projectile-tests--render-settings))

(defun my-projectile-tests--set-threads ()
  "Set the number of threads used for a batch run."
  (interactive)
  (let ((threads (read-number "Test threads: "
                              (or (plist-get my-projectile-tests-batch-settings :threads)
                                  (my-projectile-tests--default-threads)))))
    (unless (and (integerp threads) (<= 1 threads my-projectile-tests--max-threads))
      (user-error "Test threads must be an integer from 1 to %d"
                  my-projectile-tests--max-threads))
    (setq my-projectile-tests-batch-settings
          (plist-put my-projectile-tests-batch-settings :threads threads))
    (my-projectile-tests--render-settings)))

(defun my-projectile-tests--set-filter ()
  "Set a case-sensitive substring that test names must contain."
  (interactive)
  (setq my-projectile-tests-batch-settings
        (plist-put my-projectile-tests-batch-settings :filter
                   (read-string "Include tests containing (empty for all): "
                                (plist-get my-projectile-tests-batch-settings :filter))))
  (my-projectile-tests--render-settings))

(defun my-projectile-tests--run-unit ()
  "Run unit tests using the settings displayed in this buffer."
  (interactive)
  (my-projectile-tests--start-batch 'unit my-projectile-tests--project-root))

(defun my-projectile-tests--run-integration ()
  "Run integration tests using the settings displayed in this buffer."
  (interactive)
  (my-projectile-tests--start-batch 'integration my-projectile-tests--project-root))

(define-key my-projectile-tests-settings-mode-map (kbd "s") #'my-projectile-tests--toggle-slow)
(define-key my-projectile-tests-settings-mode-map (kbd "t") #'my-projectile-tests--set-threads)
(define-key my-projectile-tests-settings-mode-map (kbd "f") #'my-projectile-tests--set-filter)
(define-key my-projectile-tests-settings-mode-map (kbd "d") #'my-projectile-tests--toggle-discovery)
(define-key my-projectile-tests-settings-mode-map (kbd "r") #'my-projectile-tests--toggle-fresh)
(define-key my-projectile-tests-settings-mode-map (kbd "u") #'my-projectile-tests--run-unit)
(define-key my-projectile-tests-settings-mode-map (kbd "i") #'my-projectile-tests--run-integration)
(define-key my-projectile-tests-settings-mode-map (kbd "q") #'quit-window)

(define-derived-mode my-projectile-tests-mode special-mode "Projectile Tests"
  "Major mode for parallel Google Test results.
Press TAB or RET on a failed test to expand its rerun logs."
  (setq-local outline-regexp "^\\* ")
  (outline-minor-mode 1))

(define-key my-projectile-tests-mode-map (kbd "TAB") #'outline-toggle-children)
(define-key my-projectile-tests-mode-map (kbd "RET") #'outline-toggle-children)

(defun my-projectile-tests--delete-outdir (batch)
  "Delete BATCH's runner output directory if it still exists."
  (when-let* ((outdir (my-projectile-tests--batch-outdir batch)))
    (when (file-directory-p outdir)
      (ignore-errors (delete-directory outdir t)))))

(defun my-projectile-tests--discard (batch)
  "Stop BATCH's runner and delete its output directory."
  (let ((process (my-projectile-tests--batch-process batch)))
    (if (not (process-live-p process))
        (my-projectile-tests--delete-outdir batch)
      (set-process-filter process #'ignore)
      (set-process-sentinel process
                            (lambda (proc _event)
                              (unless (process-live-p proc)
                                (my-projectile-tests--delete-outdir batch))))
      (ignore-errors (process-send-string process "stop\n"))
      (run-at-time 2 nil (lambda ()
                           (when (process-live-p process)
                             (delete-process process))
                           (my-projectile-tests--delete-outdir batch))))))

(defun my-projectile-tests--cancel ()
  "Stop the batch runner and delete its output when its buffer is killed."
  (when-let* ((batch my-projectile-tests--current-batch))
    (unless (my-projectile-tests--batch-finished batch)
      (setf (my-projectile-tests--batch-cancelled batch) t)
      (message "Cancelled Projectile batch tests"))
    (my-projectile-tests--discard batch)))

(defun my-projectile-tests--kill-runners ()
  "Kill batch runners and delete their output directories."
  (dolist (buffer (buffer-list))
    (when-let* ((batch (buffer-local-value 'my-projectile-tests--current-batch buffer)))
      (let ((process (my-projectile-tests--batch-process batch)))
        (when (process-live-p process)
          (delete-process process)))
      (my-projectile-tests--delete-outdir batch))))

(add-hook 'kill-emacs-hook #'my-projectile-tests--kill-runners)

(defun my-projectile-tests--source-text (batch final)
  "Describe where BATCH's test list came from; FINAL if it has finished."
  (pcase (my-projectile-tests--batch-source batch)
    ("cache"
     (format "Test list and timings cached %s%s; press d in the batch settings to rediscover"
             (my-projectile-tests--batch-cache-time batch)
             (if-let* ((timed (my-projectile-tests--batch-timed batch)))
                 (format "; updated timing cache (%d/%d timed)" (car timed) (cdr timed))
               "")))
    ("listed"
     (if (my-projectile-tests--batch-cache batch)
         (if-let* ((timed (my-projectile-tests--batch-timed batch)))
             (format "Discovered tests; recorded timings for %d/%d tests"
                     (car timed) (cdr timed))
           (unless final "Discovered tests; recording timings"))
       "Run without cache: discovered tests; round-robin scheduling; timing cache disabled"))))

(defun my-projectile-tests--render (batch &optional final)
  "Update BATCH's result buffer; fold failures if FINAL is non-nil."
  (when (buffer-live-p (my-projectile-tests--batch-buffer batch))
    (with-current-buffer (my-projectile-tests--batch-buffer batch)
      (let ((inhibit-read-only t)
            (threads (my-projectile-tests--batch-threads batch))
            (passed 0) (failed 0) (skipped 0) (unrun 0))
        (dolist (test (my-projectile-tests--batch-tests batch))
          (pcase (gethash test (my-projectile-tests--batch-results batch))
            ('passed (cl-incf passed))
            ('failed (cl-incf failed))
            ('skipped (cl-incf skipped))
            (_ (cl-incf unrun))))
        (erase-buffer)
        (insert (propertize (format "%s tests: "
                                    (capitalize (symbol-name
                                                 (my-projectile-tests--batch-kind batch))))
                            'face '(:inherit font-lock-function-name-face
                                             :weight bold))
                (propertize (format "%d/%d passed"
                                    passed (length (my-projectile-tests--batch-tests batch)))
                            'face 'success))
        (when (my-projectile-tests--batch-tests batch)
          (insert (propertize (format " (%d failed, %d skipped, %d not run)"
                                      failed skipped unrun)
                              'face (if (or (> failed 0) (> unrun 0)) 'warning 'shadow))))
        (when final
          (insert (format " in %.2f seconds" (my-projectile-tests--batch-elapsed batch)))
          (when-let* ((estimate (my-projectile-tests--batch-estimate batch)))
            (insert (propertize (format " (estimated %.2f)" estimate) 'face 'shadow))))
        (insert "\n")
        (when-let* ((line (my-projectile-tests--source-text batch final)))
          (insert (propertize line 'face 'shadow) "\n"))
        (if final
            (if (my-projectile-tests--batch-tests batch)
                (insert (format "%d %s on %d logical CPUs\n"
                                threads (if (= threads 1) "thread" "threads")
                                (my-projectile-tests--batch-cpus batch)))
              (insert (if (my-projectile-tests--batch-errors batch)
                          "No tests ran\n"
                        "No tests matched the batch settings\n")))
          (insert (cond
                   ((my-projectile-tests--batch-reruns-total batch)
                    (format "Rerunning failures: %d/%d completed\n"
                            (my-projectile-tests--batch-reruns-done batch)
                            (my-projectile-tests--batch-reruns-total batch)))
                   (threads
                    (format "Running tests: %d/%d completed on %d %s%s\n"
                            (my-projectile-tests--batch-tests-done batch)
                            (length (my-projectile-tests--batch-tests batch))
                            threads (if (= threads 1) "thread" "threads")
                            (if-let* ((estimate (my-projectile-tests--batch-estimate batch)))
                                (format ", estimated %.1f seconds" estimate)
                              "")))
                   ((and (not (my-projectile-tests--batch-discover batch))
                         (when-let* ((cache (my-projectile-tests--batch-cache batch)))
                           (file-exists-p cache)))
                    "Loading cached test cases...\n")
                   (t "Discovering test cases...\n"))))
        (when (and final (my-projectile-tests--batch-tests batch))
          (dolist (test (my-projectile-tests--batch-tests batch))
            (when (eq (gethash test (my-projectile-tests--batch-results batch)) 'failed)
              (insert (propertize (format "\n* FAILED: %s\n" test) 'face 'error)
                      (or (gethash test (my-projectile-tests--batch-logs batch))
                          "No rerun output was captured.\n")))))
        (when final
          (dolist (error-text (reverse (my-projectile-tests--batch-errors batch)))
            (insert (propertize "\n* Batch error\n" 'face 'error)
                    error-text "\n")))
        (goto-char (point-min))
        (when final
          (outline-hide-body))))))

(defun my-projectile-tests--send (batch &rest fields)
  "Send the runner command made of FIELDS to BATCH's runner."
  (dolist (field fields)
    (when (string-match-p "[\t\n\r]" field)
      (error "Runner command field contains a tab or line break: %S" field)))
  (process-send-string (my-projectile-tests--batch-process batch)
                       (concat (string-join fields "\t") "\n")))

(defun my-projectile-tests--finish (batch &optional stop)
  "Finish BATCH and report any errors.
Tell the runner to exit, killing its test processes if STOP is non-nil.
Its log files remain until the result buffer is killed or reused."
  (unless (my-projectile-tests--batch-finished batch)
    (setf (my-projectile-tests--batch-elapsed batch)
          (float-time (time-since (my-projectile-tests--batch-start-time batch)))
          (my-projectile-tests--batch-finished batch) t)
    (let ((process (my-projectile-tests--batch-process batch)))
      (when (process-live-p process)
        (ignore-errors (process-send-string process (if stop "stop\n" "quit\n")))))
    (my-projectile-tests--render batch t)
    (if (my-projectile-tests--batch-errors batch)
        (message "Projectile batch tests finished with errors; see %s"
                 (buffer-name (my-projectile-tests--batch-buffer batch)))
      (message "Projectile batch tests finished; see %s"
               (buffer-name (my-projectile-tests--batch-buffer batch))))))

(defun my-projectile-tests--log-text (path &optional full)
  "Return the log file at PATH preceded by its name.
Unless FULL, keep only the last `my-projectile-tests--log-excerpt-size'
characters."
  (cond
   ((or (null path) (string-empty-p path)) "No log was written.\n")
   ((not (file-readable-p path)) (format "Log file is missing: %s\n" path))
   (t (with-temp-buffer
        (insert-file-contents path)
        (when (and (not full)
                   (> (buffer-size) my-projectile-tests--log-excerpt-size))
          (delete-region (point-min)
                         (- (point-max) my-projectile-tests--log-excerpt-size))
          (goto-char (point-min))
          (insert "[...]\n"))
        (goto-char (point-min))
        (insert (format "Log file: %s\n\n" path))
        (buffer-string)))))

(defun my-projectile-tests--read-xml (batch names path)
  "Record BATCH results for NAMES from Google Test XML at PATH."
  (let ((expected (make-hash-table :test 'equal))
        (reported (make-hash-table :test 'equal))
        (document (car (xml-parse-file path))))
    (unless (eq (xml-node-name document) 'testsuites)
      (error "Invalid Google Test XML root in %s" path))
    (dolist (name names)
      (puthash name t expected))
    (dolist (suite (xml-get-children document 'testsuite))
      (dolist (case-node (xml-get-children suite 'testcase))
        (let ((name (concat (xml-get-attribute case-node 'classname) "."
                            (xml-get-attribute case-node 'name))))
          (when (gethash name expected)
            (puthash name t reported)
            (puthash name
                     (cond ((or (xml-get-children case-node 'failure)
                                (xml-get-children case-node 'error)) 'failed)
                           ((or (xml-get-children case-node 'skipped)
                                (equal (xml-get-attribute case-node 'status) "notrun"))
                            'skipped)
                           (t 'passed))
                     (my-projectile-tests--batch-results batch))))))
    (dolist (name names)
      (unless (gethash name reported)
        (puthash name 'not-run (my-projectile-tests--batch-results batch))))))

(defun my-projectile-tests--chunk-finished (batch names exit-code xml log
                                                  &optional start-error)
  "Record BATCH's test process for NAMES, which exited with EXIT-CODE.
Read results from Google Test XML and keep LOG for errors.  START-ERROR
is the reason the runner could not start the process."
  (condition-case err
      (cond (start-error (error "Could not start test process: %s" start-error))
            ((file-exists-p xml) (my-projectile-tests--read-xml batch names xml))
            (t (error "Test process produced no Google Test XML")))
    (error
     (dolist (name names)
       (puthash name 'not-run (my-projectile-tests--batch-results batch)))
     (push (format "Test result error for %d %s: %s\n%s"
                   (length names) (if (cdr names) "tests" "test")
                   (error-message-string err)
                   (if start-error "" (my-projectile-tests--log-text log)))
           (my-projectile-tests--batch-errors batch))))
  (when (and (not start-error)
             (not (zerop exit-code))
             (cl-every (lambda (name)
                         (eq (gethash name (my-projectile-tests--batch-results batch))
                             'passed))
                       names))
    (push (format "Test process exited with status %d without reported failures:\n%s"
                  exit-code (my-projectile-tests--log-text log))
          (my-projectile-tests--batch-errors batch)))
  (cl-incf (my-projectile-tests--batch-tests-done batch) (length names))
  (my-projectile-tests--render batch))

(defun my-projectile-tests--start-reruns (batch)
  "Ask BATCH's runner to rerun each failed test with logging enabled."
  (let ((failed (cl-remove-if-not
                 (lambda (name)
                   (eq (gethash name (my-projectile-tests--batch-results batch)) 'failed))
                 (my-projectile-tests--batch-tests batch))))
    (setf (my-projectile-tests--batch-rerun-names batch) (vconcat failed)
          (my-projectile-tests--batch-reruns-total batch) (length failed)
          (my-projectile-tests--batch-reruns-done batch) 0)
    (if (null failed)
        (my-projectile-tests--finish batch)
      (my-projectile-tests--render batch)
      (cl-loop for name in failed for id from 0
               do (my-projectile-tests--send batch "rerun" (number-to-string id) name)))))

(defun my-projectile-tests--rerun-name (batch id)
  "Return the test name of BATCH's rerun ID."
  (aref (my-projectile-tests--batch-rerun-names batch) (string-to-number id)))

(defun my-projectile-tests--rerun-finished (batch id text)
  "Record TEXT as the log of BATCH's rerun ID; finish after the last one."
  (puthash (my-projectile-tests--rerun-name batch id) text
           (my-projectile-tests--batch-logs batch))
  (cl-incf (my-projectile-tests--batch-reruns-done batch))
  (if (= (my-projectile-tests--batch-reruns-done batch)
         (my-projectile-tests--batch-reruns-total batch))
      (my-projectile-tests--finish batch)
    (my-projectile-tests--render batch)))

(defun my-projectile-tests--handle-event (batch fields)
  "Update BATCH for the runner event made of FIELDS."
  (pcase fields
    (`("hello" ,_ ,version)
     (unless (equal version my-projectile-tests--runner-protocol)
       (error "emacs-test-runner speaks protocol %s but %s is required; rebuild it (see README.md)"
              version my-projectile-tests--runner-protocol)))
    (`("test" ,name)
     (push name (my-projectile-tests--batch-discovering batch)))
    (`("discovered" ,_total ,_selected ,threads ,source ,estimate)
     (setf (my-projectile-tests--batch-tests batch)
           (nreverse (my-projectile-tests--batch-discovering batch))
           (my-projectile-tests--batch-discovering batch) nil
           (my-projectile-tests--batch-threads batch) (string-to-number threads)
           (my-projectile-tests--batch-source batch) source
           (my-projectile-tests--batch-estimate batch)
           (let ((milliseconds (string-to-number estimate)))
             (and (> milliseconds 0) (/ milliseconds 1000.0))))
     (when (equal source "cache")
       (setf (my-projectile-tests--batch-cache-time batch)
             (if-let* ((attributes (file-attributes
                                    (my-projectile-tests--batch-cache batch))))
                 (format-time-string "%Y-%m-%d %H:%M"
                                     (file-attribute-modification-time attributes))
               "at an unknown time")))
     (my-projectile-tests--render batch))
    (`("cache-saved" ,timed ,total)
     (setf (my-projectile-tests--batch-timed batch)
           (cons (string-to-number timed) (string-to-number total))))
    (`("cache-failed" ,message)
     (push message (my-projectile-tests--batch-errors batch)))
    (`("chunk-done" ,exit ,xml ,log . ,names)
     (my-projectile-tests--chunk-finished batch names (string-to-number exit) xml log))
    (`("chunk-failed" ,message . ,names)
     (my-projectile-tests--chunk-finished batch names -1 nil nil message))
    (`("run-finished")
     (my-projectile-tests--start-reruns batch))
    (`("rerun-done" ,id ,exit ,log)
     (my-projectile-tests--rerun-finished
      batch id (format "Rerun exit status: %s\n%s"
                       exit (my-projectile-tests--log-text log t))))
    (`("rerun-failed" ,id ,message)
     (push (format "Could not rerun %s: %s"
                   (my-projectile-tests--rerun-name batch id) message)
           (my-projectile-tests--batch-errors batch))
     (my-projectile-tests--rerun-finished
      batch id (format "Rerun could not start: %s\n" message)))
    (`("discover-failed" ,message ,log)
     (push (format "Could not list Google Test cases: %s\n%s"
                   message (if (string-empty-p log) ""
                             (my-projectile-tests--log-text log)))
           (my-projectile-tests--batch-errors batch))
     (my-projectile-tests--finish batch t))
    (`("error" ,message)
     (push (format "emacs-test-runner: %s" message)
           (my-projectile-tests--batch-errors batch))
     (my-projectile-tests--finish batch t))
    (_
     (push (format "Unexpected emacs-test-runner output: %s"
                   (string-join fields "\t"))
           (my-projectile-tests--batch-errors batch)))))

(defun my-projectile-tests--runner-filter (process output)
  "Handle each complete line of OUTPUT from the runner PROCESS."
  (let* ((batch (process-get process 'my-projectile-tests--batch))
         (lines (split-string (concat (my-projectile-tests--batch-pending batch) output)
                              "\n")))
    (setf (my-projectile-tests--batch-pending batch) (car (last lines)))
    (dolist (line (butlast lines))
      (setq line (string-remove-suffix "\r" line))
      (unless (or (string-empty-p line)
                  (my-projectile-tests--batch-finished batch)
                  (my-projectile-tests--batch-cancelled batch))
        (condition-case err
            (my-projectile-tests--handle-event batch (split-string line "\t"))
          (error
           (push (format "Could not handle runner output %S: %s"
                         line (error-message-string err))
                 (my-projectile-tests--batch-errors batch))
           (my-projectile-tests--finish batch t)))))))

(defun my-projectile-tests--runner-sentinel (process event)
  "Report a runner PROCESS that exits, with EVENT, before its batch is done."
  (unless (process-live-p process)
    (let ((batch (process-get process 'my-projectile-tests--batch)))
      (unless (or (my-projectile-tests--batch-finished batch)
                  (my-projectile-tests--batch-cancelled batch))
        (push (format "emacs-test-runner exited unexpectedly: %s" (string-trim event))
              (my-projectile-tests--batch-errors batch))
        (my-projectile-tests--finish batch t)))))

(defun my-projectile-tests--runner ()
  "Return the emacs-test-runner executable or explain how to build it."
  (let ((program my-projectile-tests-runner-program))
    (unless (and program (file-regular-p program) (file-executable-p program))
      (user-error "emacs-test-runner is not built (%s); see \"Build emacs-test-runner\" in %s"
                  program (expand-file-name "README.md" user-emacs-directory)))
    program))

(defun my-projectile-tests--start-runner (batch program)
  "Start PROGRAM as BATCH's runner and send it the batch settings."
  (let ((flags (my-projectile-tests--batch-flags batch))
        (filter (my-projectile-tests--batch-filter batch))
        (cache (my-projectile-tests--batch-cache batch))
        (default-directory (my-projectile-tests--batch-root batch)))
    (setf (my-projectile-tests--batch-outdir batch)
          (make-temp-file "emacs-test-runner-" t))
    (when cache
      (make-directory (file-name-directory cache) t))
    (let ((process (make-process
                    :name "emacs-test-runner"
                    :command (list program)
                    :connection-type 'pipe
                    :coding 'utf-8-unix
                    :noquery t
                    :filter #'my-projectile-tests--runner-filter
                    :sentinel #'my-projectile-tests--runner-sentinel)))
      (process-put process 'my-projectile-tests--batch batch)
      (setf (my-projectile-tests--batch-process batch) process))
    (my-projectile-tests--send batch "exe" (my-projectile-tests--batch-executable batch))
    (my-projectile-tests--send batch "cwd" (my-projectile-tests--batch-root batch))
    (my-projectile-tests--send batch "outdir" (my-projectile-tests--batch-outdir batch))
    (dolist (flag flags)
      (my-projectile-tests--send batch "arg" flag))
    (dolist (flag (remove "-disableLogs" flags))
      (my-projectile-tests--send batch "rerun-arg" flag))
    (my-projectile-tests--send batch "threads"
                               (number-to-string
                                (my-projectile-tests--batch-thread-limit batch)))
    (unless (string-empty-p filter)
      (my-projectile-tests--send batch "filter" filter))
    (when (my-projectile-tests--batch-exclude-slow batch)
      (my-projectile-tests--send batch "exclude-slow"))
    (when cache
      (my-projectile-tests--send batch "cache" cache)
      (when (my-projectile-tests--batch-discover batch)
        (my-projectile-tests--send batch "rediscover")))
    (my-projectile-tests--send batch "run")))

(defun my-projectile-tests--start-batch (kind root)
  "Discover and run KIND's Google Test cases in parallel from ROOT."
  (unless (memq kind '(unit integration))
    (user-error "Test kind must be unit or integration"))
  (unless root
    (user-error "No project selected for batch tests"))
  (let* ((runner (my-projectile-tests--runner))
         (settings my-projectile-tests-batch-settings)
         (fresh (plist-get settings :fresh))
         (tnt (my-projectile-tests--tnt-p root))
         (executable (if tnt
                         (expand-file-name
                          (concat "Local\\Bin\\Win64-Dll\\release\\"
                                  (alist-get kind my-projectile-tests--tnt-executables))
                          root)
                       (expand-file-name
                        (read-file-name "Google Test executable: " root nil t))))
         (buffer (get-buffer-create (format "*Projectile %s batch tests*" kind)))
         (batch (make-my-projectile-tests--batch
                 :kind kind :root root :executable executable
                 :flags (when tnt '("-disableLogs" "-disableCallstackResolution"))
                 :buffer buffer :cpus (num-processors)
                 :thread-limit (or (plist-get settings :threads)
                                   (my-projectile-tests--default-threads))
                 :exclude-slow (plist-get settings :exclude-slow)
                 :filter (or (plist-get settings :filter) "")
                 :discover (and (not fresh) (plist-get settings :discover))
                 :cache (unless fresh (my-projectile-tests--cache-file executable))
                 :results (make-hash-table :test 'equal)
                 :logs (make-hash-table :test 'equal)
                 :tests-done 0)))
    (unless (and (file-regular-p executable) (file-executable-p executable))
      (user-error "Google Test executable not found or not executable: %s" executable))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer) root)))
    (with-current-buffer buffer
      (when my-projectile-tests--current-batch
        (unless (my-projectile-tests--batch-finished my-projectile-tests--current-batch)
          (user-error "A batch is already running in %s" (buffer-name buffer)))
        (my-projectile-tests--discard my-projectile-tests--current-batch))
      (my-projectile-tests-mode)
      (setq my-projectile-tests--current-batch batch)
      (add-hook 'kill-buffer-hook #'my-projectile-tests--cancel nil t))
    (my-projectile-tests--render batch)
    (pop-to-buffer buffer)
    (setf (my-projectile-tests--batch-start-time batch) (current-time))
    (condition-case err
        (my-projectile-tests--start-runner batch runner)
      (error
       (push (format "Could not start emacs-test-runner: %s" (error-message-string err))
             (my-projectile-tests--batch-errors batch))
       (my-projectile-tests--finish batch t)))
    batch))

(defun my-projectile-test-project-batch (&optional kind)
  "Open batch settings for the current project.
Press s to exclude SLOW tests, t to choose the thread count, and f to
include only test names containing a case-sensitive substring.  Press
d to toggle discovery mode, which lists the tests again and records how
long each takes; other batches reuse that list and balance the tests
across threads by their recorded durations.  Press u or i to launch
unit or integration tests.  Press r to toggle run without cache: always discover
tests and distribute them round-robin without reading or writing a
timing cache.  Run without cache overrides discovery mode.  Settings persist across
Emacs sessions.  Batches run through emacs-test-runner, which must be
built first (see README.md).
Failed cases are rerun with logging enabled; press TAB on a failure in
the result buffer to inspect its logs.
When called with KIND from Lisp, run that kind directly."
  (interactive)
  (let ((root (projectile-acquire-root)))
    (if kind
        (my-projectile-tests--start-batch kind root)
      (let ((buffer (get-buffer-create
                     (format "*Projectile batch settings: %s*"
                             (file-name-nondirectory (directory-file-name root))))))
        (with-current-buffer buffer
          (my-projectile-tests-settings-mode)
          (setq my-projectile-tests--project-root root)
          (my-projectile-tests--render-settings))
        (pop-to-buffer buffer)))))

(provide 'my-projectile-tests)
;;; my-projectile-tests.el ends here
