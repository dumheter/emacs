;;; p4-tests.el --- Tests for the local Perforce integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs --batch -Q -L local-packages -l p4-tests.el -f ert-run-tests-batch-and-exit.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'p4)

(ert-deftest p4-test-server-version ()
  (let ((p4-server-version-cache nil)
        (calls 0)
        (response "Server version: P4D/LINUX26X86_64/2024.2.123456/123456\n"))
    (cl-letf (((symbol-function 'p4-current-server-port)
               (lambda () "test:1666"))
              ((symbol-function 'p4-run)
               (lambda (args)
                 (should (equal args '("info")))
                 (cl-incf calls)
                 (insert response)
                 (goto-char (point-min))
                 0)))
      (should (= 2024 (p4-server-version)))
      (should (= 2024 (p4-server-version)))
      (should (= calls 1))
      (setq p4-server-version-cache nil
            response "Server version: P4D/LINUX26X86_64/2023.1/123456\n")
      (should (= 2023 (p4-server-version)))
      (setq p4-server-version-cache nil
            response "Server version: unknown\n")
      (should-error (p4-server-version))
      (should-not p4-server-version-cache))))

(ert-deftest p4-test-cl-from-buffer ()
  (let ((kill-ring nil)
        (interprogram-cut-function nil)
        (action "edit"))
    (cl-letf (((symbol-function 'p4-run)
               (lambda (args)
                 (should (equal (car args) "fstat"))
                 (insert (format
                          "... depotFile //depot/test.txt\n... action %s\n... change 42\n"
                          action))
                 (goto-char (point-min))
                 0)))
      (with-temp-buffer
        (setq buffer-file-name "test.txt")
        (should (equal (p4-cl-from-buffer) "We are working on CL 42."))
        (setq action "add")
        (should (equal (p4-cl-from-buffer) "We are working on CL 42."))
        (setq action "delete")
        (should-error (p4-cl-from-buffer) :type 'user-error)))))

(ert-deftest p4-test-arg-completion ()
  (let ((completion (make-p4-completion)))
    (setf (p4-completion-completion-fn completion)
          (lambda (_string _predicate _action) "branch"))
    (should (equal (funcall (p4-arg-completion-builder completion)
                            "p4 br" nil nil)
                   "p4 branch"))))

(ert-deftest p4-test-diff-patience ()
  (let ((file (make-temp-file "p4-patience-client-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "new\n"))
          (cl-letf (((symbol-function 'p4-run)
                     (lambda (args)
                       (pcase args
                         (`("fstat" "-T" ,_ ,_)
                          (insert (format
                                   "... depotFile //depot/test.txt\n... clientFile %s\n... haveRev 2\n"
                                   file))
                          (goto-char (point-min))
                          0)
                         (`("print" "-q" "-o" ,target "//depot/test.txt#2")
                          (with-temp-file target (insert "old\n"))
                          0)
                         (_ (error "Unexpected p4 arguments: %S" args))))))
            (p4-diff-patience file)
            (with-current-buffer p4--diff-patience-buffer-name
              (should (eq major-mode 'p4-diff-mode))
              (should (eq revert-buffer-function #'p4--diff-patience-revert))
              (should (string-match-p
                       (regexp-quote "--- //depot/test.txt#2")
                       (buffer-string)))
              (should (string-match-p
                       (regexp-quote (concat "+++ " file))
                       (buffer-string)))
              (should (string-match-p (regexp-quote "-old\n+new")
                                      (buffer-string))))))
      (when (get-buffer p4--diff-patience-buffer-name)
        (kill-buffer p4--diff-patience-buffer-name))
      (delete-file file))))

(defun p4-tests--wait-for (predicate)
  "Wait at most ten seconds for PREDICATE to become non-nil."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(defmacro p4-tests--with-status-client (responses &rest body)
  "Run BODY with an isolated fake P4 client using RESPONSES.
Each response maps a P4 command to Lisp evaluated in a child Emacs.
COMMANDS records arguments and filename lists; PROCESSES records children."
  (declare (indent 1))
  `(let ((p4-executable (expand-file-name invocation-name invocation-directory))
         (p4-do-find-file t)
         (p4-update-status-pending-alist nil)
         (p4-update-status-timeout 600)
         (p4-status-command-timeout 3)
         (p4-update-status-process-buffer " *P4 test update status*")
         (responses ,responses)
         (make-process-function (symbol-function 'make-process))
         commands processes)
     (cl-letf (((symbol-function 'p4-call-process)
                (lambda (&rest _)
                  (error "Automatic status must not call P4 synchronously")))
               ((symbol-function 'process-send-string)
                (lambda (&rest _)
                  (error "Automatic status must not write into process pipes")))
               ((symbol-function 'make-process)
                (lambda (&rest args)
                  (let ((process (apply make-process-function args)))
                    (push process processes)
                    process))))
       (let ((p4-modify-args-function
              (lambda (args)
                (push (cons args
                            (when (member "-x" args)
                              (with-temp-buffer
                                (insert-file-contents (nth 2 args))
                                (buffer-string))))
                      commands)
                (list "--batch" "-Q" "--eval"
                      (or (cdr (assoc (car (last args)) responses))
                          (error "Unexpected P4 command: %S" args))))))
         (unwind-protect
             (progn ,@body)
           (setq p4-do-find-file nil
                 p4-update-status-pending-alist nil)
           (dolist (process processes)
             (when (process-live-p process) (delete-process process))
             (when (buffer-live-p (process-buffer process))
               (kill-buffer (process-buffer process)))))))))

(ert-deftest p4-test-status-settings-are-asynchronous ()
  (p4-tests--with-status-client
      '(("set" . "(princ \"P4CLIENT=test-client (config 'test')\\nP4PORT=test:1666\\nP4CHARSET=utf8 (set)\\n\")")
        ("opened" . "(princ \"info: //depot/test.txt#3 - edit change 42 (text)\\n\")"))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "test.txt"))
      (p4-update-status)
      (should (process-live-p p4--status-settings-process))
      (insert "Still editable\n")
      (p4-tests--wait-for (lambda () (eq p4-vc-status 'edit)))
      (should (= p4-vc-revision 3))
      (should-not p4--status-settings-process)
      (should-not p4-update-status-pending-alist)
      (should (equal (mapcar (lambda (entry) (car (last (car entry))))
                            (reverse commands))
                     '("set" "opened")))
      (should (equal (cdr (car commands))
                     (concat buffer-file-name "\n")))
      (should-not (file-exists-p (nth 2 (caar commands)))))))

(ert-deftest p4-test-status-without-client-is-skipped ()
  (let (messages)
    (p4-tests--with-status-client
        '(("set" . "(princ \"P4CONFIG=.p4config (config 'noconfig')\\nP4PORT=perforce:1666 (set)\\n\")"))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "outside-workspace.log"))
          (p4-update-status)
          (p4-tests--wait-for (lambda () (not p4--status-settings-process)))
          (should-not p4-update-status-pending-alist)
          (should-not p4-vc-status)
          (should-not p4-mode)
          (should (= (length commands) 1))
          (should-not messages))))))

(ert-deftest p4-test-status-hung-settings-do-not-block ()
  (p4-tests--with-status-client '(("set" . "(sleep-for 60)"))
    (let ((p4-status-command-timeout 0.3)
          (ticks 0)
          timer)
      (unwind-protect
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "merged-result.txt"))
            (setq timer (run-at-time 0 0.02 (lambda () (cl-incf ticks))))
            (p4-update-status)
            (should (process-live-p p4--status-settings-process))
            (insert "Merged result remains editable\n")
            (p4-tests--wait-for (lambda () (not p4--status-settings-process)))
            (should (> ticks 0))
            (should-not p4-update-status-pending-alist)
            (should (process-get (car processes) 'p4-status-timed-out))
            (should-not (process-live-p (car processes)))
            (should-not (buffer-live-p (process-buffer (car processes)))))
        (when timer (cancel-timer timer))))))

(ert-deftest p4-test-status-opening-merged-result-does-not-wait ()
  (p4-tests--with-status-client '(("set" . "(sleep-for 60)"))
    (let ((p4-status-command-timeout 1)
          (file (make-temp-file "p4-merged-result-"))
          buffer)
      (unwind-protect
          (progn
            (setq buffer (find-file-noselect file))
            (with-current-buffer buffer
              (should (process-live-p p4--status-settings-process))
              (insert "Resolved merge\n")
              (p4-tests--wait-for (lambda () (not p4--status-settings-process)))
              (should (equal (buffer-string) "Resolved merge\n"))
              (should-not p4-vc-status))
            (should (process-get (car processes) 'p4-status-timed-out)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-file file)))))

(ert-deftest p4-test-status-opened-and-have-use-filename-files ()
  (p4-tests--with-status-client
      '(("opened" . "(progn (princ \"info: //depot/edited.txt#3 - edit change 42 (text)\\nerror: synced.txt - file(s) not opened on this client.\\n\") (kill-emacs 1))")
        ("have" . "(princ \"info: //depot/synced.txt#7 - synced.txt\\n\")"))
    (let ((edited (generate-new-buffer " *P4 test edited*"))
          (synced (generate-new-buffer " *P4 test synced*")))
      (unwind-protect
          (progn
            (with-current-buffer edited
              (setq buffer-file-name (expand-file-name "edited.txt")))
            (with-current-buffer synced
              (setq buffer-file-name (expand-file-name "synced.txt")))
            (setq p4-update-status-pending-alist
                  (list (list "P4CHARSET=utf8 (set)\n" (seconds-to-time 0)
                              (list edited synced))))
            (p4-maybe-start-update-statuses)
            (p4-tests--wait-for
             (lambda () (eq (buffer-local-value 'p4-vc-status synced) 'sync)))
            (should (eq (buffer-local-value 'p4-vc-status edited) 'edit))
            (should (= (buffer-local-value 'p4-vc-revision edited) 3))
            (should (= (buffer-local-value 'p4-vc-revision synced) 7))
            (should-not p4-update-status-pending-alist)
            (should (= (length commands) 2))
            (dolist (entry commands)
              (should (equal (cl-subseq (car entry) 0 2) '("-s" "-x")))
              (should-not (equal (nth 2 (car entry)) "-"))
              (should-not (file-exists-p (nth 2 (car entry)))))
            (should (equal (cdr (car commands))
                           (concat (buffer-file-name synced) "\n")))
            (should (equal (cdr (cadr commands))
                           (concat (buffer-file-name edited) "\n"
                                   (buffer-file-name synced) "\n"))))
        (kill-buffer edited)
        (kill-buffer synced)))))

(ert-deftest p4-test-status-unopened-and-untracked-files-are-quiet ()
  (dolist (response '(("warning" 0) ("error" 1)))
    (let ((prefix (car response))
          (exit-status (cadr response))
          messages)
      (p4-tests--with-status-client
          `(("opened" . ,(format
                          "(progn (princ %S) (kill-emacs %d))"
                          (format
                           "%s: synced.txt - file(s) not opened on this client.\n%s: untracked.json - file(s) not opened on this client.\nexit: %d\n"
                           prefix prefix exit-status)
                          exit-status))
            ("have" . ,(format
                        "(progn (princ %S) (kill-emacs %d))"
                        (format
                         "%s: untracked.json - file(s) not on client.\ninfo: //depot/synced.txt#7 - synced.txt\nexit: %d\n"
                         prefix exit-status)
                        exit-status)))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "synced.txt"))
            (let ((synced (current-buffer)))
              (with-temp-buffer
                (setq buffer-file-name (expand-file-name "untracked.json"))
                (p4-update-mode (current-buffer) 'edit 1)
                (setq p4-update-status-pending-alist
                      (list (list "" (seconds-to-time 0)
                                  (list synced (current-buffer)))))
                (p4-maybe-start-update-statuses)
                (p4-tests--wait-for
                 (lambda () (not p4-update-status-pending-alist)))
                (should (eq (buffer-local-value 'p4-vc-status synced) 'sync))
                (should (= (buffer-local-value 'p4-vc-revision synced) 7))
                (should-not p4-vc-status)
                (should-not p4-vc-revision)
                (should-not p4-mode)
                (should (= (length commands) 2))
                (should (equal (cdr (car commands))
                               (concat buffer-file-name "\n"
                                       (buffer-file-name synced) "\n")))
                (should-not messages)))))))))

(ert-deftest p4-test-status-unexpected-warnings-remain-visible ()
  (dolist (command '("opened" "have"))
    (let (messages)
      (p4-tests--with-status-client
          `(("opened" . ,(if (equal command "opened")
                            "(progn (princ \"warning: Perforce password invalid or unset.\\nexit: 1\\n\") (kill-emacs 1))"
                          "(princ \"warning: test.txt - file(s) not opened on this client.\\nexit: 0\\n\")"))
            ("have" . "(progn (princ \"warning: Perforce password invalid or unset.\\nexit: 1\\n\") (kill-emacs 1))"))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "test.txt"))
            (setq p4-update-status-pending-alist
                  (list (list "" (seconds-to-time 0) (list (current-buffer)))))
            (p4-maybe-start-update-statuses)
            (p4-tests--wait-for
             (lambda () (not (get-buffer p4-update-status-process-buffer))))
            (should (equal (car (last (caar commands))) command))
            (should (equal (cl-third (car p4-update-status-pending-alist))
                           (list (current-buffer))))
            (should (cl-some
                     (lambda (text)
                       (string-match-p
                        "P4 background status .*Perforce password" text))
                     messages))))))))

(ert-deftest p4-test-status-hung-queries-keep-delayed-retries ()
  (dolist (command '("opened" "have"))
    (p4-tests--with-status-client
        `(("opened" . ,(if (equal command "opened")
                          "(sleep-for 60)"
                        "(princ \"error: merged.txt - file(s) not opened on this client.\\n\")"))
          ("have" . "(sleep-for 60)"))
      (let ((p4-status-command-timeout
             (if (equal command "opened") 0.3 3)))
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "merged.txt"))
          (setq p4-update-status-pending-alist
                (list (list "" (seconds-to-time 0) (list (current-buffer)))))
          (p4-maybe-start-update-statuses)
          (setq p4-status-command-timeout 0.3)
          (p4-tests--wait-for
           (lambda ()
             (and (process-get (car processes) 'p4-status-timed-out)
                  (not (process-live-p (car processes))))))
          (should (equal (car (last (caar commands))) command))
          (should (equal (cl-third (car p4-update-status-pending-alist))
                         (list (current-buffer))))
          (should-not p4-vc-status)
          (should-not (get-buffer p4-update-status-process-buffer))
          (dolist (entry commands)
            (should-not (file-exists-p (nth 2 (car entry)))))
          (let ((count (length commands)))
            (p4-maybe-start-update-statuses)
            (should (= (length commands) count))))))))

(ert-deftest p4-test-status-settings-cancel-on-buffer-kill ()
  (p4-tests--with-status-client '(("set" . "(sleep-for 60)"))
    (let ((buffer (generate-new-buffer " *P4 test merged result*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq buffer-file-name (expand-file-name "merged-result.txt"))
              (p4-update-status))
            (should (process-live-p (car processes)))
            (kill-buffer buffer)
            (should-not (process-live-p (car processes)))
            (should-not (buffer-live-p (process-buffer (car processes))))
            (should-not p4-update-status-pending-alist))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest p4-test-status-settings-ignore-renamed-buffer ()
  (p4-tests--with-status-client
      '(("set" . "(princ \"P4PORT=test:1666\\n\")"))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "old.txt"))
      (p4-update-status)
      (setq buffer-file-name (expand-file-name "new.txt"))
      (p4-tests--wait-for (lambda () (not p4--status-settings-process)))
      (should-not p4-update-status-pending-alist)
      (should (= (length commands) 1)))))

(ert-deftest p4-test-status-start-failure-cleans-filename-file ()
  (p4-tests--with-status-client '(("opened" . "(sleep-for 60)"))
    (let ((p4-executable (expand-file-name "nonexistent-p4-client")))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "merged.txt"))
        (setq p4-update-status-pending-alist
              (list (list "" (seconds-to-time 0) (list (current-buffer)))))
        (p4-maybe-start-update-statuses)
        (should-not processes)
        (should-not (get-buffer p4-update-status-process-buffer))
        (should (= (length commands) 1))
        (should-not (file-exists-p (nth 2 (caar commands))))
        (should (equal (cl-third (car p4-update-status-pending-alist))
                       (list (current-buffer))))))))

(ert-deftest p4-test-status-setup-failure-stops-created-process ()
  (p4-tests--with-status-client '(("opened" . "(sleep-for 60)"))
    (cl-letf (((symbol-function 'process-send-eof)
               (lambda (&rest _) (error "Fake EOF failure"))))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "merged.txt"))
        (setq p4-update-status-pending-alist
              (list (list "" (seconds-to-time 0) (list (current-buffer)))))
        (p4-maybe-start-update-statuses)
        (should (= (length processes) 1))
        (should-not (process-live-p (car processes)))
        (should-not (get-buffer p4-update-status-process-buffer))
        (should-not (file-exists-p (nth 2 (caar commands))))
        (should-not (memq (process-get (car processes) 'p4-status-timer)
                          timer-list))))))

(ert-deftest p4-test-status-forced-refresh-cancels-old-settings ()
  (p4-tests--with-status-client '(("set" . "(sleep-for 60)"))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "merged.txt"))
      (p4-update-status)
      (let ((old p4--status-settings-process))
        (p4-update-status t)
        (should-not (process-live-p old))
        (should-not (buffer-live-p (process-buffer old)))
        (should (process-live-p p4--status-settings-process))
        (should-not (eq old p4--status-settings-process))
        (should-not p4-update-status-pending-alist)))))

(provide 'p4-tests)

;;; p4-tests.el ends here
