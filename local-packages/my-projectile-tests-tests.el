;;; my-projectile-tests-tests.el --- Batch test settings tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs --batch -Q --eval '(package-initialize)' -L local-packages
;; -l local-packages/my-projectile-tests-tests.el -f ert-run-tests-batch-and-exit.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'my-projectile-tests)
(require 'savehist)

(ert-deftest my-projectile-tests-settings-commands ()
  (let ((my-projectile-tests-batch-settings nil))
    (with-temp-buffer
      (my-projectile-tests-settings-mode)
      (setq my-projectile-tests--project-root "C:\\src\\TnT\\")
      (my-projectile-tests--render-settings)
      (should (string-match "PROJECTILE  /  TEST RUNNER" (buffer-string)))
      (should (get-text-property (1+ (match-beginning 0)) 'face))
      (should (string-match-p "SLOW tests: *EXCLUDE" (buffer-string)))
      (should (string-match-p "Threads: *auto" (buffer-string)))
      (should (string-match-p "Discovery mode: *OFF" (buffer-string)))
      (should (string-match-p "Run without cache: *OFF" (buffer-string)))
      (should (eq (lookup-key my-projectile-tests-settings-mode-map (kbd "u"))
                  #'my-projectile-tests--run-unit))
      (should (eq (lookup-key my-projectile-tests-settings-mode-map (kbd "r"))
                  #'my-projectile-tests--toggle-fresh))
      (my-projectile-tests--toggle-discovery)
      (call-interactively (lookup-key (current-local-map) (kbd "r")))
      (should (plist-get my-projectile-tests-batch-settings :fresh))
      (should (plist-get my-projectile-tests-batch-settings :discover))
      (should (string-match-p "Run without cache: *ON (discover; no cache or timings)"
                              (buffer-string)))
      (should (string-match-p "Discovery mode: *IGNORED" (buffer-string)))
      (my-projectile-tests--toggle-fresh)
      (should-not (plist-get my-projectile-tests-batch-settings :fresh))
      (should (plist-get my-projectile-tests-batch-settings :discover))
      (should (string-match-p "Discovery mode: *ON" (buffer-string)))
      (my-projectile-tests--toggle-slow)
      (should (plist-member my-projectile-tests-batch-settings :exclude-slow))
      (should-not (plist-get my-projectile-tests-batch-settings :exclude-slow))
      (should (string-match-p "SLOW tests: *INCLUDE" (buffer-string)))
      (my-projectile-tests--toggle-slow)
      (should (plist-get my-projectile-tests-batch-settings :exclude-slow))
      (should (string-match-p "SLOW tests: *EXCLUDE" (buffer-string)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 3))
                ((symbol-function 'read-string) (lambda (&rest _) "Fast")))
        (my-projectile-tests--set-threads)
        (my-projectile-tests--set-filter))
      (should (= (plist-get my-projectile-tests-batch-settings :threads) 3))
      (should (equal (plist-get my-projectile-tests-batch-settings :filter) "Fast"))
      (should (string-match-p "Include tests containing: *Fast" (buffer-string)))
      (should (string-match-p "\\[u\\]  UNIT TESTS" (buffer-string)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 0)))
        (should-error (my-projectile-tests--set-threads) :type 'user-error))
      (should (= (plist-get my-projectile-tests-batch-settings :threads) 3)))))

(ert-deftest my-projectile-tests-settings-highlight-only-active-overrides ()
  (cl-letf (((symbol-function 'num-processors) (lambda () 8)))
    (dolist (case
             '((nil nil)
               ((:exclude-slow t :threads 4 :filter "" :discover nil
                 :fresh nil :disable-logs t :disable-callstack-resolution t
                 :gtest-repeat 1 :repeat 1)
                nil)
               ((:exclude-slow nil :threads 3 :filter "Fast" :discover t
                 :disable-logs nil :disable-callstack-resolution nil
                 :gtest-repeat 2 :repeat 3)
                ("s" "t" "f" "g" "p" "d" "l" "c"))
               ((:threads 5) ("t"))
               ((:exclude-slow nil) ("s"))
               ((:fresh t :discover t) ("r"))
               ((:fresh t :discover nil) ("r"))))
      (let ((my-projectile-tests-batch-settings (car case))
            (warning-keys (cadr case))
            (rows 0))
        (with-temp-buffer
          (my-projectile-tests-settings-mode)
          (setq my-projectile-tests--project-root "C:\\src\\TnT\\")
          (my-projectile-tests--render-settings)
          (while (re-search-forward "^  \\[\\([stfgpdrlc]\\)\\]  " nil t)
            (let ((key (match-string 1)))
              (forward-char 29)
              (should (eq (get-text-property (point) 'face)
                          (if (member key warning-keys) 'warning 'shadow)))
              (setq rows (1+ rows))))
          (should (= rows 9)))))))

(ert-deftest my-projectile-tests-warns-on-high-windows-thread-count ()
  (let ((my-projectile-tests-batch-settings '(:threads 30)))
    (with-temp-buffer
      (my-projectile-tests-settings-mode)
      (setq my-projectile-tests--project-root "C:\\src\\TnT\\")
      (my-projectile-tests--render-settings)
      (should (string-match-p "Threads: *30" (buffer-string)))
      (when (eq system-type 'windows-nt)
        (should (string-match-p "High thread counts can exhaust Emacs process pipes"
                                (buffer-string)))))))

(ert-deftest my-projectile-tests-select-tests ()
  (let ((batch (make-my-projectile-tests--batch :exclude-slow t :filter "Fast"))
        (tests '("SLOW_Suite.Fast" "Suite.SLOW_Fast" "Suite.Fast"
                 "Instance/SLOW_Suite.Fast" "Suite.Fast/0" "Suite.Other")))
    (should (equal (my-projectile-tests--select-tests batch tests)
                   '("Suite.Fast" "Suite.Fast/0")))
    (setf (my-projectile-tests--batch-exclude-slow batch) nil)
    (should (equal (my-projectile-tests--select-tests batch tests)
                   (butlast tests)))
    (setf (my-projectile-tests--batch-filter batch) "")
    (should (equal (my-projectile-tests--select-tests batch tests) tests))
    (setf (my-projectile-tests--batch-filter batch) "Missing")
    (should-not (my-projectile-tests--select-tests batch tests))))

(ert-deftest my-projectile-tests-thread-limit ()
  (with-temp-buffer
    (my-projectile-tests-mode)
    (let ((batch (make-my-projectile-tests--batch
                  :kind 'unit :buffer (current-buffer) :cpus 8
                  :thread-limit 3 :threads-done 0
                  :results (make-hash-table :test 'equal)))
          (groups nil))
      (cl-letf (((symbol-function 'my-projectile-tests--start-process)
                 (lambda (_batch stage names)
                   (should (eq stage 'thread))
                   (push names groups))))
        (my-projectile-tests--start-threads batch '("a" "b" "c" "d" "e")))
      (should (= (my-projectile-tests--batch-threads batch) 3))
      (should (equal (sort (apply #'append groups) #'string<)
                     '("a" "b" "c" "d" "e")))
      (should (string-match-p "Running threads: 0/3" (buffer-string)))
      (setf (my-projectile-tests--batch-thread-limit batch) 10)
      (cl-letf (((symbol-function 'my-projectile-tests--start-process)
                 (lambda (&rest _) nil)))
        (my-projectile-tests--start-threads batch '("a" "b")))
      (should (= (my-projectile-tests--batch-threads batch) 2)))))

(ert-deftest my-projectile-tests-pipe-exhaustion-remains-visible ()
  (with-temp-buffer
    (my-projectile-tests-mode)
    (let* ((names '("Suite.One" "Suite.Two" "Suite.Three"))
           (batch (make-my-projectile-tests--batch
                   :kind 'unit :buffer (current-buffer) :cpus 48
                   :threads 3 :threads-done 0 :active 0
                   :tests names :start-time (current-time)
                   :results (make-hash-table :test 'equal)
                   :logs (make-hash-table :test 'equal))))
      (dolist (name names)
        (my-projectile-tests--thread-finished
         batch (list name) nil -1 "Creating pipe: Too many open files"))
      (should (my-projectile-tests--batch-finished batch))
      (should (= (length (my-projectile-tests--batch-errors batch)) 1))
      (should (string-match-p "0/3 passed (0 failed, 0 skipped, 3 not run)"
                              (buffer-string)))
      (should (string-match-p "Lower the thread count (t)" (buffer-string)))
      (should (= (cl-count ?* (buffer-string)) 1))
      (dolist (name names)
        (should (eq (gethash name (my-projectile-tests--batch-results batch))
                    'not-run))))))

(ert-deftest my-projectile-tests-launch-from-settings ()
  (let ((root "C:\\src\\TnT\\")
        (launched nil)
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-acquire-root) (lambda () root))
                  ((symbol-function 'pop-to-buffer) (lambda (target &rest _) target))
                  ((symbol-function 'my-projectile-tests--start-batch)
                   (lambda (kind project-root)
                     (push (list kind project-root) launched))))
          (setq buffer (my-projectile-test-project-batch))
          (with-current-buffer buffer
            (should (eq major-mode 'my-projectile-tests-settings-mode))
            (call-interactively (lookup-key (current-local-map) (kbd "u")))
            (call-interactively (lookup-key (current-local-map) (kbd "i"))))
          (should (equal launched `((integration ,root) (unit ,root)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my-projectile-tests-launch-captures-settings ()
  (dolist (case '((nil t nil)
                  ((:exclude-slow nil :discover nil) nil nil)
                  ((:exclude-slow t :discover t) t t)))
    (let ((my-projectile-tests-batch-settings
           (append (car case) '(:threads 4 :filter "Fast")))
          (buffer nil))
      (unwind-protect
          (cl-letf (((symbol-function 'my-projectile-tests--runner)
                     (lambda () "runner.exe"))
                    ((symbol-function 'read-file-name) (lambda (&rest _) "tests.exe"))
                    ((symbol-function 'file-regular-p) (lambda (&rest _) t))
                    ((symbol-function 'file-executable-p) (lambda (&rest _) t))
                    ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
                    ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                    ((symbol-function 'my-projectile-tests--start-runner)
                     (lambda (&rest _) nil)))
            (let ((batch (my-projectile-tests--start-batch 'unit "C:\\src\\Other\\")))
              (setq buffer (my-projectile-tests--batch-buffer batch))
              (should (= (my-projectile-tests--batch-thread-limit batch) 4))
              (should (eq (my-projectile-tests--batch-exclude-slow batch) (cadr case)))
              (should (equal (my-projectile-tests--batch-filter batch) "Fast"))
              (should (eq (my-projectile-tests--batch-discover batch) (caddr case)))
              (should (equal (my-projectile-tests--batch-cache batch)
                             (my-projectile-tests--cache-file
                              (my-projectile-tests--batch-executable batch))))
              (setf (my-projectile-tests--batch-finished batch) t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my-projectile-tests-settings-survive-savehist ()
  (let ((savehist-file (make-temp-file "projectile-tests-history-"))
        (savehist-additional-variables '(my-projectile-tests-batch-settings))
        (my-projectile-tests-batch-settings
         '(:exclude-slow t :threads 3 :filter "Fast" :discover t :fresh t)))
    (unwind-protect
        (progn
          (savehist-save)
          (setq my-projectile-tests-batch-settings nil)
          (load savehist-file nil t)
          (should (equal my-projectile-tests-batch-settings
                         '(:exclude-slow t :threads 3 :filter "Fast"
                           :discover t :fresh t))))
      (delete-file savehist-file))))

(ert-deftest my-projectile-tests-cached-timings-refreshed ()
  (let ((batch (make-my-projectile-tests--batch
                :source "cache" :cache-time "2026-10-02 09:38")))
    (should-not (string-match-p "updated timing cache"
                                (my-projectile-tests--source-text batch nil)))
    (my-projectile-tests--handle-event batch '("cache-saved" "3022" "3030"))
    (should (equal (my-projectile-tests--batch-timed batch) '(3022 . 3030)))
    (should (string-match-p "updated timing cache (3022/3030 timed)"
                            (my-projectile-tests--source-text batch t)))
    (should (string-match-p "press d"
                            (my-projectile-tests--source-text batch t)))))

(ert-deftest my-projectile-tests-fresh-launch-captures-settings ()
  (dolist (kind '(unit integration))
    (dolist (discover '(nil t))
      (let ((my-projectile-tests-batch-settings
             (list :exclude-slow t :threads 4 :filter "Fast"
                   :discover discover :fresh t))
            (buffer nil))
        (unwind-protect
            (cl-letf (((symbol-function 'my-projectile-tests--runner)
                       (lambda () "runner.exe"))
                      ((symbol-function 'read-file-name) (lambda (&rest _) "tests.exe"))
                      ((symbol-function 'file-regular-p) (lambda (&rest _) t))
                      ((symbol-function 'file-executable-p) (lambda (&rest _) t))
                      ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
                      ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                      ((symbol-function 'my-projectile-tests--cache-file)
                       (lambda (&rest _) (ert-fail "Fresh run requested a cache path")))
                      ((symbol-function 'my-projectile-tests--start-runner)
                       (lambda (&rest _) nil)))
              (let ((batch (my-projectile-tests--start-batch kind "C:\\src\\Other\\")))
                (setq buffer (my-projectile-tests--batch-buffer batch))
                (should (eq (my-projectile-tests--batch-kind batch) kind))
                (should (= (my-projectile-tests--batch-thread-limit batch) 4))
                (should (my-projectile-tests--batch-exclude-slow batch))
                (should (equal (my-projectile-tests--batch-filter batch) "Fast"))
                (should-not (my-projectile-tests--batch-cache batch))
                (should-not (my-projectile-tests--batch-discover batch))
                (should (eq (plist-get my-projectile-tests-batch-settings :discover)
                            discover))
                (should (string-match-p "Discovering test cases"
                                        (with-current-buffer buffer (buffer-string))))
                (setf (my-projectile-tests--batch-finished batch) t)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest my-projectile-tests-runner-cache-optional ()
  (dolist (cache '(nil "C:\\cache\\tests.etr"))
    (dolist (discover '(nil t))
      (let ((batch (make-my-projectile-tests--batch
                    :root default-directory :executable "tests.exe"
                    :flags '("-disableLogs" "-disableCallstackResolution")
                    :thread-limit 4 :filter "Fast" :exclude-slow t
                    :cache cache :discover discover))
            (commands nil)
            (directories nil))
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (&rest _) "C:\\temp\\test-runner\\"))
                  ((symbol-function 'make-directory)
                   (lambda (directory &rest _) (push directory directories)))
                  ((symbol-function 'make-process) (lambda (&rest _) 'runner))
                  ((symbol-function 'process-put) (lambda (&rest _) nil))
                  ((symbol-function 'my-projectile-tests--send)
                   (lambda (_batch &rest fields) (push fields commands))))
          (my-projectile-tests--start-runner batch "runner.exe"))
        (setq commands (nreverse commands))
        (should (equal (car (last commands)) '("run")))
        (should (member '("threads" "4") commands))
        (should (member '("filter" "Fast") commands))
        (should (member '("exclude-slow") commands))
        (should (member '("arg" "-disableLogs") commands))
        (should-not (member '("rerun-arg" "-disableLogs") commands))
        (should (member '("rerun-arg" "-disableCallstackResolution") commands))
        (if cache
            (progn
              (should (equal directories (list (file-name-directory cache))))
              (should (member (list "cache" cache) commands))
              (should (eq (not (null (member '("rediscover") commands))) discover)))
          (should-not directories)
          (should-not (assoc "cache" commands))
          (should-not (member '("rediscover") commands)))))))

(ert-deftest my-projectile-tests-fresh-results-describe-disabled-cache ()
  (with-temp-buffer
    (my-projectile-tests-mode)
    (let ((batch (make-my-projectile-tests--batch
                  :kind 'unit :buffer (current-buffer) :cpus 8
                  :results (make-hash-table :test 'equal)
                  :tests-done 0 :elapsed 1)))
      (cl-letf (((symbol-function 'file-exists-p)
                 (lambda (&rest _) (ert-fail "Fresh run inspected a cache"))))
        (my-projectile-tests--render batch)
        (should (string-match-p "Discovering test cases" (buffer-string)))
        (my-projectile-tests--handle-event batch '("test" "Suite.Fast"))
        (my-projectile-tests--handle-event batch '("discovered" "1" "1" "1" "listed" "0"))
        (should-not (my-projectile-tests--batch-estimate batch))
        (dolist (final '(nil t))
          (my-projectile-tests--render batch final)
          (should (string-match-p "Run without cache: discovered tests; round-robin scheduling"
                                  (buffer-string)))
          (should (string-match-p "timing cache disabled" (buffer-string)))
          (should-not (string-match-p "recording timings\\|recorded timings\\|estimated"
                                      (buffer-string))))))))

(ert-deftest my-projectile-tests-discovery-results-describe-cache ()
  (let ((batch (make-my-projectile-tests--batch :source "listed" :cache "timings.etr")))
    (should (equal (my-projectile-tests--source-text batch nil)
                   "Discovered tests; recording timings"))
    (should-not (my-projectile-tests--source-text batch t))
    (my-projectile-tests--handle-event batch '("cache-saved" "2" "3"))
    (should (equal (my-projectile-tests--source-text batch t)
                   "Discovered tests; recorded timings for 2/3 tests"))))

(provide 'my-projectile-tests-tests)
;;; my-projectile-tests-tests.el ends here
