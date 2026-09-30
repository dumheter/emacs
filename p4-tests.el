;;; p4-tests.el --- Tests for the local Perforce integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs --batch -Q -L . -l p4-tests.el -f ert-run-tests-batch-and-exit.

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

(provide 'p4-tests)

;;; p4-tests.el ends here
