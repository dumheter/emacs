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
      (should (string-match-p "Threads: auto" (buffer-string)))
      (should (eq (lookup-key my-projectile-tests-settings-mode-map (kbd "u"))
                  #'my-projectile-tests--run-unit))
      (my-projectile-tests--toggle-slow)
      (should (plist-get my-projectile-tests-batch-settings :exclude-slow))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 3))
                ((symbol-function 'read-string) (lambda (&rest _) "Fast")))
        (my-projectile-tests--set-threads)
        (my-projectile-tests--set-filter))
      (should (= (plist-get my-projectile-tests-batch-settings :threads) 3))
      (should (equal (plist-get my-projectile-tests-batch-settings :filter) "Fast"))
      (should (string-match-p "Include tests containing: Fast" (buffer-string)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 0)))
        (should-error (my-projectile-tests--set-threads) :type 'user-error))
      (should (= (plist-get my-projectile-tests-batch-settings :threads) 3)))))

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
  (let ((my-projectile-tests-batch-settings
         '(:exclude-slow t :threads 4 :filter "Fast"))
        (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) "tests.exe"))
                  ((symbol-function 'file-regular-p) (lambda (&rest _) t))
                  ((symbol-function 'file-executable-p) (lambda (&rest _) t))
                  ((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
                  ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'my-projectile-tests--start-process)
                   (lambda (_batch stage) (should (eq stage 'listing)))))
          (let ((batch (my-projectile-tests--start-batch 'unit "C:\\src\\Other\\")))
            (setq buffer (my-projectile-tests--batch-buffer batch))
            (should (= (my-projectile-tests--batch-thread-limit batch) 4))
            (should (my-projectile-tests--batch-exclude-slow batch))
            (should (equal (my-projectile-tests--batch-filter batch) "Fast"))
            (setf (my-projectile-tests--batch-finished batch) t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest my-projectile-tests-settings-survive-savehist ()
  (let ((savehist-file (make-temp-file "projectile-tests-history-"))
        (savehist-additional-variables '(my-projectile-tests-batch-settings))
        (my-projectile-tests-batch-settings
         '(:exclude-slow t :threads 3 :filter "Fast")))
    (unwind-protect
        (progn
          (savehist-save)
          (setq my-projectile-tests-batch-settings nil)
          (load savehist-file nil t)
          (should (equal my-projectile-tests-batch-settings
                         '(:exclude-slow t :threads 3 :filter "Fast"))))
      (delete-file savehist-file))))

(provide 'my-projectile-tests-tests)
;;; my-projectile-tests-tests.el ends here
