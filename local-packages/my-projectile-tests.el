;;; my-projectile-tests.el --- Projectile test commands and parallel gtest runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run unit or integration tests directly, or discover and run their Google
;; Test cases in parallel.  The batch buffer summarizes results and folds
;; each failed case's verbose rerun output under its own heading.  The final
;; summary includes the total elapsed time.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'projectile)
(require 'xml)

(defconst my-projectile-tests--tnt-executables
  '((unit . "Extension.BattlefieldOnline.Runtime.Test_Win64_release_Dll.exe")
    (integration . "Extension.BattlefieldOnline.Runtime.IntegrationTest_Win64_release_Dll.exe")))

(defvar my-projectile-test-project-unit-cmd-map (make-hash-table :test 'equal)
  "Last unit test command used in each project compilation directory.")

(defvar my-projectile-test-project-integration-cmd-map (make-hash-table :test 'equal)
  "Last integration test command used in each project compilation directory.")

(defun my-projectile-tests--tnt-p (root)
  "Return non-nil if ROOT is a TnT project root."
  (string= (file-name-nondirectory (directory-file-name root)) "TnT"))

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
  kind root executable flags buffer cpus workers tests results logs
  errors shards-done rerun-queue reruns-total reruns-done active processes
  start-time elapsed finished cancelled)

(defvar-local my-projectile-tests--current-batch nil)

(define-derived-mode my-projectile-tests-mode special-mode "Projectile Tests"
  "Major mode for parallel Google Test results.
Press TAB or RET on a failed test to expand its rerun logs."
  (setq-local outline-regexp "^\\* ")
  (outline-minor-mode 1))

(define-key my-projectile-tests-mode-map (kbd "TAB") #'outline-toggle-children)
(define-key my-projectile-tests-mode-map (kbd "RET") #'outline-toggle-children)

(defun my-projectile-tests--cancel ()
  "Stop processes when their batch results buffer is killed."
  (when-let* ((batch my-projectile-tests--current-batch))
    (unless (my-projectile-tests--batch-finished batch)
      (setf (my-projectile-tests--batch-cancelled batch) t)
      (dolist (process (my-projectile-tests--batch-processes batch))
        (set-process-sentinel process #'ignore)
        (when (process-live-p process)
          (delete-process process))
        (when-let* ((xml (process-get process 'my-projectile-tests--xml)))
          (when (file-exists-p xml)
            (delete-file xml)))
        (when (buffer-live-p (process-buffer process))
          (kill-buffer (process-buffer process))))
      (message "Cancelled Projectile batch tests"))))

(defun my-projectile-tests--render (batch &optional final)
  "Update BATCH's result buffer; fold failures if FINAL is non-nil."
  (when (buffer-live-p (my-projectile-tests--batch-buffer batch))
    (with-current-buffer (my-projectile-tests--batch-buffer batch)
      (let ((inhibit-read-only t)
            (passed 0) (failed 0) (skipped 0) (unrun 0))
        (dolist (test (my-projectile-tests--batch-tests batch))
          (pcase (gethash test (my-projectile-tests--batch-results batch))
            ('passed (cl-incf passed))
            ('failed (cl-incf failed))
            ('skipped (cl-incf skipped))
            (_ (cl-incf unrun))))
        (erase-buffer)
        (insert (format "%s tests: %d/%d passed"
                        (capitalize (symbol-name (my-projectile-tests--batch-kind batch)))
                        passed (length (my-projectile-tests--batch-tests batch))))
        (when (my-projectile-tests--batch-tests batch)
          (insert (format " (%d failed, %d skipped, %d not run)" failed skipped unrun)))
        (when final
          (insert (format " in %.2f seconds" (my-projectile-tests--batch-elapsed batch))))
        (insert "\n")
        (if final
            (if (my-projectile-tests--batch-workers batch)
                (insert (format "%d shards on %d logical CPUs (%d %s)\n"
                                (my-projectile-tests--batch-workers batch)
                                (my-projectile-tests--batch-cpus batch)
                                (my-projectile-tests--batch-workers batch)
                                (if (= (my-projectile-tests--batch-workers batch) 1)
                                    "worker" "workers")))
              (insert "No shards started\n"))
          (insert (cond
                   ((my-projectile-tests--batch-reruns-total batch)
                    (format "Rerunning failures: %d/%d completed\n"
                            (my-projectile-tests--batch-reruns-done batch)
                            (my-projectile-tests--batch-reruns-total batch)))
                   ((my-projectile-tests--batch-tests batch)
                    (format "Running shards: %d/%d completed\n"
                            (my-projectile-tests--batch-shards-done batch)
                            (my-projectile-tests--batch-workers batch)))
                   (t "Discovering test cases...\n"))))
        (when (and final (my-projectile-tests--batch-tests batch))
          (dolist (test (my-projectile-tests--batch-tests batch))
            (when (eq (gethash test (my-projectile-tests--batch-results batch)) 'failed)
              (insert (format "\n* FAILED: %s\n" test)
                      (or (gethash test (my-projectile-tests--batch-logs batch))
                          "No rerun output was captured.\n")))))
        (when final
          (dolist (error-text (reverse (my-projectile-tests--batch-errors batch)))
            (insert "\n* Batch error\n" error-text "\n")))
        (goto-char (point-min))
        (when final
          (outline-hide-body))))))

(defun my-projectile-tests--finish (batch)
  "Finish BATCH and report any errors."
  (setf (my-projectile-tests--batch-elapsed batch)
        (float-time (time-since (my-projectile-tests--batch-start-time batch)))
        (my-projectile-tests--batch-finished batch) t)
  (my-projectile-tests--render batch t)
  (if (my-projectile-tests--batch-errors batch)
      (message "Projectile batch tests finished with errors; see %s"
               (buffer-name (my-projectile-tests--batch-buffer batch)))
    (message "Projectile batch tests finished; see %s"
             (buffer-name (my-projectile-tests--batch-buffer batch)))))

(defun my-projectile-tests--parse-list (output)
  "Return enabled Google Test case names found in discovery OUTPUT."
  (let (suite tests)
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match "^\\([^[:space:]#]+\\.\\)\\(?:[[:space:]]*#.*\\)?[[:space:]]*$" line)
        (setq suite (match-string 1 line)))
       ((and suite
             (string-match "^  \\([^[:space:]#]+\\)\\(?:[[:space:]]*#.*\\)?[[:space:]]*$" line))
        (let ((test (match-string 1 line)))
          (unless (or (string-match-p "\\(?:\\`\\|/\\)DISABLED_" suite)
                      (string-prefix-p "DISABLED_" test))
            (push (concat suite test) tests))))))
    (delete-dups (nreverse tests))))

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

(defun my-projectile-tests--start-process (batch stage &optional names)
  "Start BATCH process for STAGE, running NAMES for a shard or rerun."
  (let* ((output (generate-new-buffer " *projectile-tests-output*"))
         (xml (when (eq stage 'shard) (make-temp-file "projectile-tests-" nil ".xml")))
         (default-directory (my-projectile-tests--batch-root batch))
         (args (append
                (if (eq stage 'rerun)
                    (remove "-disableLogs" (my-projectile-tests--batch-flags batch))
                  (my-projectile-tests--batch-flags batch))
                (pcase stage
                  ('listing '("--gtest_list_tests"))
                  ('shard (list (concat "--gtest_filter=" (mapconcat #'identity names ":"))
                                (concat "--gtest_output=xml:" xml)))
                  ('rerun (list (concat "--gtest_filter=" (car names))))))))
    (condition-case err
        (let ((process (make-process
                        :name "projectile-tests"
                        :buffer output
                        :command (cons (my-projectile-tests--batch-executable batch) args)
                        :noquery t
                        :sentinel #'my-projectile-tests--sentinel)))
          (process-put process 'my-projectile-tests--batch batch)
          (process-put process 'my-projectile-tests--stage stage)
          (process-put process 'my-projectile-tests--names names)
          (process-put process 'my-projectile-tests--xml xml)
          (push process (my-projectile-tests--batch-processes batch)))
      (error
       (kill-buffer output)
       (when (and xml (file-exists-p xml))
         (delete-file xml))
       (signal (car err) (cdr err))))))

(defun my-projectile-tests--start-reruns (batch)
  "Schedule failed BATCH tests, without exceeding its worker count."
  (while (and (my-projectile-tests--batch-rerun-queue batch)
              (< (my-projectile-tests--batch-active batch)
                 (my-projectile-tests--batch-workers batch)))
    (let ((name (pop (my-projectile-tests--batch-rerun-queue batch))))
      (condition-case err
          (progn
            (my-projectile-tests--start-process batch 'rerun (list name))
            (cl-incf (my-projectile-tests--batch-active batch)))
        (error
         (push (format "Could not rerun %s: %s" name (error-message-string err))
               (my-projectile-tests--batch-errors batch))
         (cl-incf (my-projectile-tests--batch-reruns-done batch))
         (puthash name (format "Rerun could not start: %s\n" (error-message-string err))
                  (my-projectile-tests--batch-logs batch))))))
  (when (and (null (my-projectile-tests--batch-rerun-queue batch))
             (zerop (my-projectile-tests--batch-active batch)))
    (my-projectile-tests--finish batch)))

(defun my-projectile-tests--shard-finished (batch names xml exit-code output)
  "Record a BATCH shard's NAMES from XML, EXIT-CODE and OUTPUT."
  (condition-case err
      (if (and xml (file-exists-p xml))
          (my-projectile-tests--read-xml batch names xml)
        (error "Shard produced no Google Test XML"))
    (error
     (dolist (name names)
       (puthash name 'not-run (my-projectile-tests--batch-results batch)))
     (push (format "Shard result error: %s\n%s" (error-message-string err) output)
           (my-projectile-tests--batch-errors batch))))
  (when (and (not (zerop exit-code))
             (cl-every (lambda (name)
                         (eq (gethash name (my-projectile-tests--batch-results batch))
                             'passed))
                       names))
    (push (format "Shard exited with status %d without reported failures:\n%s"
                  exit-code output)
          (my-projectile-tests--batch-errors batch)))
  (cl-incf (my-projectile-tests--batch-shards-done batch))
  (if (= (my-projectile-tests--batch-shards-done batch)
         (my-projectile-tests--batch-workers batch))
      (progn
        (setf (my-projectile-tests--batch-rerun-queue batch)
              (cl-remove-if-not
               (lambda (name)
                 (eq (gethash name (my-projectile-tests--batch-results batch)) 'failed))
               (my-projectile-tests--batch-tests batch))
              (my-projectile-tests--batch-reruns-total batch)
              (length (my-projectile-tests--batch-rerun-queue batch))
              (my-projectile-tests--batch-reruns-done batch) 0)
        (my-projectile-tests--render batch)
        (my-projectile-tests--start-reruns batch))
    (unless (my-projectile-tests--batch-finished batch)
      (my-projectile-tests--render batch))))

(defun my-projectile-tests--start-shards (batch tests)
  "Partition TESTS into BATCH's workers and start each shard."
  (let* ((workers (min (length tests) (max 1 (/ (1+ (my-projectile-tests--batch-cpus batch)) 2))))
         (shards (make-vector workers nil)))
    (setf (my-projectile-tests--batch-tests batch) tests
          (my-projectile-tests--batch-workers batch) workers)
    (cl-loop for test in tests for index from 0
             do (push test (aref shards (mod index workers))))
    (dotimes (index workers)
      (let ((names (nreverse (aref shards index))))
        (condition-case err
            (my-projectile-tests--start-process batch 'shard names)
          (error
           (my-projectile-tests--shard-finished
            batch names nil -1 (error-message-string err))))))
    (unless (my-projectile-tests--batch-finished batch)
      (my-projectile-tests--render batch))))

(defun my-projectile-tests--sentinel (process _event)
  "Collect PROCESS output and advance the batch when it finishes."
  (when (memq (process-status process) '(exit signal))
    (let* ((batch (process-get process 'my-projectile-tests--batch))
           (stage (process-get process 'my-projectile-tests--stage))
           (names (process-get process 'my-projectile-tests--names))
           (xml (process-get process 'my-projectile-tests--xml))
           (code (process-exit-status process))
           (buffer (process-buffer process))
           (output (with-current-buffer buffer (buffer-string))))
      (setf (my-projectile-tests--batch-processes batch)
            (delq process (my-projectile-tests--batch-processes batch)))
      (unwind-protect
          (unless (my-projectile-tests--batch-cancelled batch)
            (pcase stage
              ('listing
               (let ((tests (and (zerop code) (my-projectile-tests--parse-list output))))
                 (if tests
                     (my-projectile-tests--start-shards batch tests)
                   (push (format "Could not list Google Test cases (exit %d):\n%s"
                                 code output)
                         (my-projectile-tests--batch-errors batch))
                   (my-projectile-tests--finish batch))))
              ('shard
               (my-projectile-tests--shard-finished batch names xml code output))
              ('rerun
               (puthash (car names)
                        (format "Rerun exit status: %d\n\n%s" code output)
                        (my-projectile-tests--batch-logs batch))
               (cl-incf (my-projectile-tests--batch-reruns-done batch))
               (cl-decf (my-projectile-tests--batch-active batch))
               (my-projectile-tests--start-reruns batch)
               (unless (my-projectile-tests--batch-finished batch)
                 (my-projectile-tests--render batch)))))
        (when (and xml (file-exists-p xml))
          (delete-file xml))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun my-projectile-test-project-batch (&optional kind)
  "Discover and run KIND's Google Test cases in parallel.
Interactively select unit or integration tests.  Half of the available
logical CPUs run disjoint shards.  Failed cases are rerun with logging
enabled; press TAB on a failure in the result buffer to inspect its logs.
The final summary reports total elapsed time in seconds."
  (interactive)
  (let* ((kind (let ((selection
                      (or kind (intern (completing-read "Batch tests (unit/integration): "
                                                       '("unit" "integration") nil t)))))
                  (unless (memq selection '(unit integration))
                    (user-error "Test kind must be unit or integration"))
                  selection))
         (root (projectile-acquire-root))
         (tnt (my-projectile-tests--tnt-p root))
         (executable (if tnt
                         (expand-file-name
                          (concat "Local\\Bin\\Win64-Dll\\release\\"
                                  (alist-get kind my-projectile-tests--tnt-executables))
                          root)
                       (read-file-name "Google Test executable: " root nil t)))
         (buffer (get-buffer-create (format "*Projectile %s batch tests*" kind)))
         (batch (make-my-projectile-tests--batch
                 :kind kind :root root :executable executable
                 :flags (when tnt '("-disableLogs" "-disableCallstackResolution"))
                 :buffer buffer :cpus (num-processors)
                 :results (make-hash-table :test 'equal)
                 :logs (make-hash-table :test 'equal)
                 :shards-done 0 :active 0)))
    (unless (and (file-regular-p executable) (file-executable-p executable))
      (user-error "Google Test executable not found or not executable: %s" executable))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer) root)))
    (with-current-buffer buffer
      (when (and my-projectile-tests--current-batch
                 (not (my-projectile-tests--batch-finished
                       my-projectile-tests--current-batch)))
        (user-error "A batch is already running in %s" (buffer-name buffer)))
      (my-projectile-tests-mode)
      (setq my-projectile-tests--current-batch batch)
      (add-hook 'kill-buffer-hook #'my-projectile-tests--cancel nil t))
    (my-projectile-tests--render batch)
    (display-buffer buffer)
    (setf (my-projectile-tests--batch-start-time batch) (current-time))
    (condition-case err
        (my-projectile-tests--start-process batch 'listing)
      (error
       (push (format "Could not start test discovery: %s" (error-message-string err))
             (my-projectile-tests--batch-errors batch))
       (my-projectile-tests--finish batch)))
    batch))

(provide 'my-projectile-tests)
;;; my-projectile-tests.el ends here
