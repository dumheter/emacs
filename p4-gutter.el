;;; p4-gutter.el --- Async Perforce change indicators -*- lexical-binding: t; -*-

;;; Commentary:
;; Shows Perforce changes for the current buffer in the left fringe.
;; Perforce is only contacted to identify the file and fetch its have revision.
;; Subsequent buffer changes are compared locally and asynchronously with Git.

;;; Code:

(require 'cl-lib)
(require 'fringe)
(require 'projectile)
(require 'subr-x)

(defgroup p4-gutter nil
  "Show Perforce changes in the fringe."
  :group 'tools
  :prefix "p4-gutter-")

(defcustom p4-gutter-executable
  (or (executable-find "p4") "p4")
  "Path to the Perforce command-line client."
  :type 'string
  :group 'p4-gutter)

(defcustom p4-gutter-git-executable
  (or (executable-find "git") "git")
  "Path to Git, used for local buffer comparisons."
  :type 'string
  :group 'p4-gutter)

(defcustom p4-gutter-project-root-name "TnT"
  "Projectile root directory name for automatic activation."
  :type 'string
  :group 'p4-gutter)

(defcustom p4-gutter-update-delay 0.35
  "Idle delay in seconds before updating after a buffer change."
  :type 'number
  :group 'p4-gutter)

(defcustom p4-gutter-p4-timeout 8
  "Seconds before an unresponsive Perforce command is terminated."
  :type 'number
  :group 'p4-gutter)

(defcustom p4-gutter-diff-timeout 5
  "Seconds before an unresponsive local Git diff is terminated."
  :type 'number
  :group 'p4-gutter)

(defcustom p4-gutter-retry-delay 15
  "Initial delay in seconds before retrying a failed command.
Retries only begin while the affected buffer is active."
  :type 'number
  :group 'p4-gutter)

(defcustom p4-gutter-maximum-retry-delay 120
  "Maximum delay in seconds between retries."
  :type 'number
  :group 'p4-gutter)

(defcustom p4-gutter-move-minimum-length 20
  "Minimum character count for recognizing an exact moved block."
  :type 'integer
  :group 'p4-gutter)

(defface p4-gutter-added
  '((t (:foreground "#3fb950" :weight bold)))
  "Face for added-line indicators."
  :group 'p4-gutter)

(defface p4-gutter-removed
  '((t (:foreground "#f85149" :weight bold)))
  "Face for removed-line indicators."
  :group 'p4-gutter)

(defface p4-gutter-changed
  '((t (:foreground "#d29922" :weight bold)))
  "Face for changed or moved-line indicators."
  :group 'p4-gutter)

(define-fringe-bitmap 'p4-gutter-bitmap-added
  [#b00000000
   #b00011000
   #b00011000
   #b01111110
   #b01111110
   #b00011000
   #b00011000
   #b00000000])

(define-fringe-bitmap 'p4-gutter-bitmap-removed
  [#b00000000
   #b00000000
   #b00000000
   #b01111110
   #b01111110
   #b00000000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'p4-gutter-bitmap-changed
  [#b00000000
   #b00000000
   #b01111110
   #b00000000
   #b01111110
   #b00000000
   #b00000000
   #b00000000])

(defvar p4-gutter-mode)

(defvar-local p4-gutter--overlays nil)
(defvar-local p4-gutter--process nil)
(defvar-local p4-gutter--update-timer nil)
(defvar-local p4-gutter--baseline-file nil)
(defvar-local p4-gutter--baseline-ready-p nil)
(defvar-local p4-gutter--file-state nil)
(defvar-local p4-gutter--status 'idle)
(defvar-local p4-gutter--generation 0)
(defvar-local p4-gutter--busy-p nil)
(defvar-local p4-gutter--dirty-during-request-p nil)
(defvar-local p4-gutter--retry-count 0)
(defvar-local p4-gutter--retry-at nil)
(defvar-local p4-gutter--retry-kind nil)

(defvar p4-gutter--last-error-time 0.0
  "Time when the last Perforce gutter error was shown.")

(defconst p4-gutter--error-message-interval 30
  "Minimum seconds between echo-area error messages.")

(defconst p4-gutter--hunk-header-regexp
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "Regexp matching a unified diff hunk header.")

(defun p4-gutter--mode-line ()
  "Return the mode-line indicator for the current buffer."
  (pcase p4-gutter--status
    ('loading " P4G...")
    ('error (propertize " P4G!" 'face 'error))
    (_ " P4G")))

(defun p4-gutter--project-root ()
  "Return the current Projectile root without signaling an error."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (fboundp 'projectile-project-root))
    (ignore-errors (projectile-project-root))))

(defun p4-gutter--tnt-project-p ()
  "Return non-nil when the current Projectile root is named TnT."
  (when-let* ((root (p4-gutter--project-root))
              (name (file-name-nondirectory
                     (directory-file-name root))))
    (string-equal-ignore-case name p4-gutter-project-root-name)))

(defun p4-gutter--eligible-buffer-p ()
  "Return non-nil when the current buffer should enable automatically."
  (and buffer-file-name
       (not (file-remote-p buffer-file-name))
       (p4-gutter--tnt-project-p)))

(defun p4-gutter--delete-file (file)
  "Delete temporary FILE if it exists."
  (when (and file (file-exists-p file))
    (ignore-errors (delete-file file))))

(defun p4-gutter--resolve-executable (executable)
  "Return a runnable path for EXECUTABLE, or nil if none can be found."
  (cond
   ((and (file-name-absolute-p executable)
         (file-executable-p executable))
    executable)
   ((executable-find executable))
   (t nil)))

(defun p4-gutter--clear-overlays ()
  "Remove all Perforce gutter overlays from the current buffer."
  (mapc #'delete-overlay p4-gutter--overlays)
  (setq p4-gutter--overlays nil))

(defun p4-gutter--cancel-update-timer ()
  "Cancel the pending buffer update timer."
  (when (timerp p4-gutter--update-timer)
    (cancel-timer p4-gutter--update-timer))
  (setq p4-gutter--update-timer nil))

(defun p4-gutter--cancel-process ()
  "Cancel the current asynchronous process."
  (let ((process p4-gutter--process))
    (setq p4-gutter--process nil)
    (when (and (processp process)
               (process-live-p process))
      (delete-process process))))

(defun p4-gutter--cleanup ()
  "Stop background work and remove temporary state."
  (cl-incf p4-gutter--generation)
  (p4-gutter--cancel-update-timer)
  (p4-gutter--cancel-process)
  (p4-gutter--delete-file p4-gutter--baseline-file)
  (setq p4-gutter--baseline-file nil
        p4-gutter--baseline-ready-p nil
        p4-gutter--file-state nil
        p4-gutter--busy-p nil
        p4-gutter--retry-at nil
        p4-gutter--retry-kind nil)
  (p4-gutter--clear-overlays))

(defun p4-gutter--process-output (buffer)
  "Return BUFFER contents, or an empty string if BUFFER is unavailable."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun p4-gutter--timeout-process (process)
  "Terminate PROCESS after its configured timeout."
  (when (and (processp process)
             (process-live-p process))
    (process-put process 'p4-gutter-timed-out t)
    (delete-process process)))

(defun p4-gutter--process-sentinel (process _event)
  "Dispatch completion for an asynchronous PROCESS."
  (when (memq (process-status process) '(exit signal failed))
    (when-let ((timer (process-get process 'p4-gutter-timeout-timer)))
      (cancel-timer timer))
    (let* ((target (process-get process 'p4-gutter-target-buffer))
           (generation (process-get process 'p4-gutter-generation))
           (callback (process-get process 'p4-gutter-callback))
           (cleanup-files (process-get process 'p4-gutter-cleanup-files))
           (stdout-buffer (process-buffer process))
           (stderr-buffer (process-get process 'p4-gutter-stderr-buffer))
           (stdout (p4-gutter--process-output stdout-buffer))
           (stderr (p4-gutter--process-output stderr-buffer))
           (exit-code (process-exit-status process))
           (timed-out (process-get process 'p4-gutter-timed-out))
           keep-files)
      (when (buffer-live-p target)
        (with-current-buffer target
          (when (eq process p4-gutter--process)
            (setq p4-gutter--process nil))
          (when (and p4-gutter-mode
                     (= generation p4-gutter--generation))
            (condition-case err
                (setq keep-files
                      (funcall callback process exit-code
                               stdout stderr timed-out))
              (error
               (p4-gutter--fail
                'refresh "internal callback error: %s"
                (error-message-string err)))))))
      (dolist (file cleanup-files)
        (unless (member file keep-files)
          (p4-gutter--delete-file file)))
      (when (buffer-live-p stdout-buffer)
        (kill-buffer stdout-buffer))
      (when (buffer-live-p stderr-buffer)
        (kill-buffer stderr-buffer)))))

(defun p4-gutter--start-process
    (name command timeout generation callback &optional cleanup-files)
  "Run COMMAND asynchronously and invoke CALLBACK when it exits.
NAME identifies the process, TIMEOUT limits its runtime, and GENERATION
prevents stale results from changing the buffer.  CLEANUP-FILES are deleted
unless CALLBACK returns a list containing a file that should be retained."
  (let ((stdout-buffer (generate-new-buffer (format " *%s-output*" name)))
        (stderr-buffer (generate-new-buffer (format " *%s-error*" name))))
    (condition-case err
        (let ((process
               (make-process
                :name name
                :buffer stdout-buffer
                :command command
                :connection-type 'pipe
                :noquery t
                :stderr stderr-buffer
                :sentinel #'p4-gutter--process-sentinel)))
          (process-put process 'p4-gutter-target-buffer (current-buffer))
          (process-put process 'p4-gutter-generation generation)
          (process-put process 'p4-gutter-callback callback)
          (process-put process 'p4-gutter-cleanup-files cleanup-files)
          (process-put process 'p4-gutter-stderr-buffer stderr-buffer)
          (process-put
           process 'p4-gutter-timeout-timer
           (run-at-time timeout nil #'p4-gutter--timeout-process process))
          (setq p4-gutter--process process)
          process)
      (error
       (when (buffer-live-p stdout-buffer)
         (kill-buffer stdout-buffer))
       (when (buffer-live-p stderr-buffer)
         (kill-buffer stderr-buffer))
       (mapc #'p4-gutter--delete-file cleanup-files)
       (funcall callback nil 127 "" (error-message-string err) nil)
       nil))))

(defun p4-gutter--error-text (stdout stderr)
  "Return a compact error description from STDOUT and STDERR."
  (let* ((text (string-trim (concat stdout "\n" stderr)))
         (single-line (replace-regexp-in-string "[\r\n\t ]+" " " text)))
    (if (string-empty-p single-line)
        "command failed without output"
      (truncate-string-to-width single-line 300 nil nil t))))

(defun p4-gutter--fail (kind format-string &rest args)
  "Record a failure of KIND described by FORMAT-STRING and ARGS."
  (let* ((now (float-time))
         (delay (min p4-gutter-maximum-retry-delay
                     (* p4-gutter-retry-delay
                        (expt 2 p4-gutter--retry-count))))
         (message-text (apply #'format format-string args)))
    (setq p4-gutter--busy-p nil
          p4-gutter--status 'error
          p4-gutter--retry-kind kind
          p4-gutter--retry-at (+ now delay)
          p4-gutter--retry-count (1+ p4-gutter--retry-count))
    (force-mode-line-update)
    (when (>= (- now p4-gutter--last-error-time)
              p4-gutter--error-message-interval)
      (setq p4-gutter--last-error-time now)
      (message "p4-gutter: %s; will retry while this buffer is active"
               message-text))))

(defun p4-gutter--finish-success (&optional rerun-if-dirty)
  "Finish a successful request.
When RERUN-IF-DIRTY is non-nil, schedule another diff if the buffer changed
while the completed process was running."
  (let ((dirty p4-gutter--dirty-during-request-p))
    (setq p4-gutter--busy-p nil
          p4-gutter--dirty-during-request-p nil
          p4-gutter--status 'ready
          p4-gutter--retry-count 0
          p4-gutter--retry-at nil
          p4-gutter--retry-kind nil)
    (force-mode-line-update)
    (when (and rerun-if-dirty dirty)
      (p4-gutter--schedule-update))))

(defun p4-gutter--begin-request ()
  "Cancel stale work and return a new request generation."
  (cl-incf p4-gutter--generation)
  (p4-gutter--cancel-process)
  (setq p4-gutter--busy-p t
        p4-gutter--dirty-during-request-p nil
        p4-gutter--status 'loading
        p4-gutter--retry-at nil)
  (force-mode-line-update)
  p4-gutter--generation)

(defun p4-gutter--tagged-value (key text)
  "Return KEY's value from Perforce tagged output TEXT."
  (when (string-match
         (format "^\\.\\.\\. %s \\([^\r\n]*\\)\r?$" (regexp-quote key))
         text)
    (match-string 1 text)))

(defun p4-gutter--untracked-output-p (text)
  "Return non-nil when Perforce TEXT means the file is untracked."
  (let ((case-fold-search t))
    (string-match-p
     (regexp-opt '("no such file(s)"
                   "not in client view"
                   "is not under client's root"
                   "file(s) not in client view"))
     text)))

(defun p4-gutter--buffer-line-count ()
  "Return the number of real text lines in the current buffer."
  (save-restriction
    (widen)
    (cond
     ((= (point-min) (point-max)) 0)
     ((eq (char-before (point-max)) ?\n)
      (line-number-at-pos (1- (point-max))))
     (t
      (line-number-at-pos (point-max))))))

(defun p4-gutter--whole-file-markers (type)
  "Return markers of TYPE for every line in the current buffer."
  (cl-loop for line from 1 to (p4-gutter--buffer-line-count)
           collect (cons line type)))

(defun p4-gutter--marker-display (type)
  "Return a fringe display string for marker TYPE."
  (pcase-let ((`(,bitmap ,face ,description)
               (pcase type
                 ('added
                  '(p4-gutter-bitmap-added p4-gutter-added
                    "Line added in Perforce workspace"))
                 ('removed
                  '(p4-gutter-bitmap-removed p4-gutter-removed
                    "Line removed from Perforce have revision"))
                 (_
                  '(p4-gutter-bitmap-changed p4-gutter-changed
                    "Line changed or moved in Perforce workspace")))))
    (propertize " "
                'display `(left-fringe ,bitmap ,face)
                'help-echo description)))

(defun p4-gutter--line-position (line)
  "Return the buffer position at the beginning of LINE."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- (max 1 line)))
      (point))))

(defun p4-gutter--apply-markers (markers)
  "Replace current gutter indicators with MARKERS.
MARKERS is a list of (LINE . TYPE) pairs."
  (let ((old-overlays p4-gutter--overlays)
        new-overlays)
    (dolist (marker markers)
      (let* ((position (p4-gutter--line-position (car marker)))
             (overlay (make-overlay position position nil t nil)))
        (overlay-put overlay 'before-string
                     (p4-gutter--marker-display (cdr marker)))
        (overlay-put overlay 'priority 1001)
        (overlay-put overlay 'p4-gutter t)
        (overlay-put overlay 'p4-gutter-type (cdr marker))
        (push overlay new-overlays)))
    (setq p4-gutter--overlays (nreverse new-overlays))
    (mapc #'delete-overlay old-overlays)))

(defun p4-gutter--parse-hunks (diff-text)
  "Parse DIFF-TEXT into a list of unified diff hunk plists."
  (with-temp-buffer
    (insert diff-text)
    (goto-char (point-min))
    (let (hunks)
      (while (re-search-forward p4-gutter--hunk-header-regexp nil t)
        (let* ((old-start (string-to-number (match-string 1)))
               (old-count (string-to-number (or (match-string 2) "1")))
               (new-start (string-to-number (match-string 3)))
               (new-count (string-to-number (or (match-string 4) "1")))
               (body-start (line-beginning-position 2))
               (body-end
                (save-excursion
                  (goto-char body-start)
                  (if (re-search-forward "^@@ " nil t)
                      (match-beginning 0)
                    (point-max))))
               old-lines
               new-lines)
          (goto-char body-start)
          (while (< (point) body-end)
            (pcase (char-after)
              (?-
               (push (buffer-substring-no-properties
                      (1+ (line-beginning-position)) (line-end-position))
                     old-lines))
              (?+
               (push (buffer-substring-no-properties
                      (1+ (line-beginning-position)) (line-end-position))
                     new-lines)))
            (forward-line 1))
          (push (list :old-start old-start
                      :old-count old-count
                      :new-start new-start
                      :new-count new-count
                      :old-lines (nreverse old-lines)
                      :new-lines (nreverse new-lines)
                      :moved nil)
                hunks)
          (goto-char body-end)))
      (nreverse hunks))))

(defun p4-gutter--significant-move-p (lines)
  "Return non-nil when LINES are substantial enough to identify as moved."
  (>= (length (string-join lines "\n"))
      p4-gutter-move-minimum-length))

(defun p4-gutter--mark-moved-hunks (hunks)
  "Mark exact delete/add pairs in HUNKS as moved."
  (let ((deletions
         (cl-remove-if-not
          (lambda (hunk)
            (and (> (plist-get hunk :old-count) 0)
                 (= (plist-get hunk :new-count) 0)))
          hunks)))
    (dolist (addition hunks)
      (when (and (= (plist-get addition :old-count) 0)
                 (> (plist-get addition :new-count) 0)
                 (p4-gutter--significant-move-p
                  (plist-get addition :new-lines)))
        (when-let ((deletion
                    (cl-find-if
                     (lambda (candidate)
                       (and (not (plist-get candidate :moved))
                            (equal (plist-get candidate :old-lines)
                                   (plist-get addition :new-lines))))
                     deletions)))
          (setf (plist-get deletion :moved) t
                (plist-get addition :moved) t))))
    hunks))

(defun p4-gutter--marker-priority (type)
  "Return display priority for marker TYPE."
  (pcase type
    ('changed 3)
    ('removed 2)
    (_ 1)))

(defun p4-gutter--diff-markers (diff-text)
  "Return line markers parsed from DIFF-TEXT."
  (let ((table (make-hash-table :test #'eql))
        (hunks (p4-gutter--mark-moved-hunks
                (p4-gutter--parse-hunks diff-text))))
    (cl-labels
        ((put-marker
          (line type)
          (let ((existing (gethash line table)))
            (when (or (null existing)
                      (> (p4-gutter--marker-priority type)
                         (p4-gutter--marker-priority existing)))
              (puthash line type table))))
         (put-range
          (start count type)
          (dotimes (offset count)
            (put-marker (+ start offset) type))))
      (dolist (hunk hunks)
        (let ((old-count (plist-get hunk :old-count))
              (new-start (plist-get hunk :new-start))
              (new-count (plist-get hunk :new-count))
              (moved (plist-get hunk :moved)))
          (cond
           (moved
            (if (> new-count 0)
                (put-range (max 1 new-start) new-count 'changed)
              (put-marker (max 1 new-start) 'changed)))
           ((= old-count 0)
            (put-range (max 1 new-start) new-count 'added))
           ((= new-count 0)
            (put-marker (max 1 new-start) 'removed))
           (t
            (put-range (max 1 new-start) new-count 'changed)))))
      (let (markers)
        (maphash (lambda (line type)
                   (push (cons line type) markers))
                 table)
        (sort markers (lambda (left right)
                        (< (car left) (car right))))))))

(defun p4-gutter--write-buffer-snapshot ()
  "Write the current buffer to a temporary file and return its path."
  (let ((snapshot (make-temp-file "p4-gutter-current-"))
        (coding-system-for-write buffer-file-coding-system)
        (write-region-inhibit-fsync t))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) snapshot nil 'silent))
    snapshot))

(defun p4-gutter--start-local-diff (generation)
  "Compare the current buffer with its baseline for GENERATION."
  (if (not (and p4-gutter--baseline-ready-p
                p4-gutter--baseline-file
                (file-exists-p p4-gutter--baseline-file)))
      (p4-gutter--fail 'refresh "the Perforce baseline is unavailable")
    (if-let ((git (p4-gutter--resolve-executable
                   p4-gutter-git-executable)))
        (condition-case err
            (let ((snapshot (p4-gutter--write-buffer-snapshot)))
              (setq p4-gutter--dirty-during-request-p nil)
              (p4-gutter--start-process
               "p4-gutter-diff"
               (list git
                     "--no-pager"
                     "diff"
                     "--no-index"
                     "--patience"
                     "--unified=0"
                     "--no-color"
                     "--no-ext-diff"
                     "--text"
                     p4-gutter--baseline-file
                     snapshot)
               p4-gutter-diff-timeout
               generation
               (lambda (_process exit-code stdout stderr timed-out)
                 (cond
                  (timed-out
                   (p4-gutter--fail
                    'diff "local diff timed out after %s seconds"
                    p4-gutter-diff-timeout))
                  ((not (memq exit-code '(0 1)))
                   (p4-gutter--fail
                    'diff "local diff failed: %s"
                    (p4-gutter--error-text stdout stderr)))
                  (t
                   (p4-gutter--apply-markers
                    (if (= exit-code 0)
                        nil
                      (p4-gutter--diff-markers stdout)))
                   (p4-gutter--finish-success t)))
                 nil)
               (list snapshot)))
          (error
           (p4-gutter--fail
            'diff "could not create a buffer snapshot: %s"
            (error-message-string err))))
      (p4-gutter--fail
       'diff "could not find executable %s"
       p4-gutter-git-executable))))

(defun p4-gutter--set-simple-file-state (state type)
  "Set file STATE and show TYPE on every current line."
  (setq p4-gutter--file-state state
        p4-gutter--baseline-ready-p nil
        p4-gutter--dirty-during-request-p nil)
  (p4-gutter--delete-file p4-gutter--baseline-file)
  (setq p4-gutter--baseline-file nil)
  (p4-gutter--apply-markers (p4-gutter--whole-file-markers type))
  (p4-gutter--finish-success))

(defun p4-gutter--handle-fstat
    (generation _process exit-code stdout stderr timed-out)
  "Handle Perforce fstat output for GENERATION."
  (let* ((output (concat stdout "\n" stderr))
         (depot-file (p4-gutter--tagged-value "depotFile" stdout))
         (have-rev (p4-gutter--tagged-value "haveRev" stdout))
         (action (downcase
                  (or (p4-gutter--tagged-value "action" stdout) ""))))
    (cond
     (timed-out
      (p4-gutter--fail
       'refresh "p4 fstat timed out after %s seconds"
       p4-gutter-p4-timeout))
     ((and (not depot-file)
           (or (= exit-code 0)
               (p4-gutter--untracked-output-p output)))
      (setq p4-gutter--file-state 'untracked
            p4-gutter--baseline-ready-p nil
            p4-gutter--dirty-during-request-p nil)
      (p4-gutter--delete-file p4-gutter--baseline-file)
      (setq p4-gutter--baseline-file nil)
      (p4-gutter--apply-markers nil)
      (p4-gutter--finish-success))
     ((not depot-file)
      (p4-gutter--fail
       'refresh "p4 fstat failed: %s"
       (p4-gutter--error-text stdout stderr)))
     ((string= action "add")
      (p4-gutter--set-simple-file-state 'added 'added))
     ((string= action "move/add")
      (p4-gutter--set-simple-file-state 'moved 'changed))
     ((member action '("delete" "move/delete"))
      (setq p4-gutter--file-state 'deleted
            p4-gutter--baseline-ready-p nil
            p4-gutter--dirty-during-request-p nil)
      (p4-gutter--delete-file p4-gutter--baseline-file)
      (setq p4-gutter--baseline-file nil)
      (p4-gutter--apply-markers
       (list (cons 1 (if (string= action "move/delete")
                         'changed
                       'removed))))
      (p4-gutter--finish-success))
     ((member have-rev '(nil "" "0" "none"))
      (p4-gutter--set-simple-file-state 'added 'added))
     (t
      (let ((baseline (make-temp-file "p4-gutter-have-")))
        (p4-gutter--start-process
         "p4-gutter-print"
         (list (or (p4-gutter--resolve-executable p4-gutter-executable)
                   p4-gutter-executable)
               "print"
               "-q"
               "-o"
               baseline
               (format "%s#%s" depot-file have-rev))
         p4-gutter-p4-timeout
         generation
         (lambda (_process print-exit-code print-stdout print-stderr
                           print-timed-out)
           (cond
            (print-timed-out
             (p4-gutter--fail
              'refresh "p4 print timed out after %s seconds"
              p4-gutter-p4-timeout)
             nil)
            ((or (/= print-exit-code 0)
                 (not (file-exists-p baseline)))
             (p4-gutter--fail
              'refresh "p4 print failed: %s"
              (p4-gutter--error-text print-stdout print-stderr))
             nil)
            (t
             (p4-gutter--delete-file p4-gutter--baseline-file)
             (setq p4-gutter--baseline-file baseline
                   p4-gutter--baseline-ready-p t
                   p4-gutter--file-state 'tracked)
             (p4-gutter--start-local-diff generation)
             (list baseline))))
         (list baseline)))))))

(defun p4-gutter-refresh ()
  "Asynchronously refresh Perforce state and gutter markers."
  (interactive)
  (cond
   ((not p4-gutter-mode)
    (when (called-interactively-p 'interactive)
      (user-error "p4-gutter-mode is not enabled")))
   ((not buffer-file-name)
    (when (called-interactively-p 'interactive)
      (user-error "Current buffer is not visiting a file")))
   ((not (p4-gutter--resolve-executable p4-gutter-executable))
    (p4-gutter--fail
     'refresh "could not find executable %s" p4-gutter-executable))
   (t
    (p4-gutter--cancel-update-timer)
    (let ((p4 (p4-gutter--resolve-executable p4-gutter-executable))
          (generation (p4-gutter--begin-request))
          (file (expand-file-name buffer-file-name)))
      (p4-gutter--start-process
       "p4-gutter-fstat"
       (list p4
             "-ztag"
             "fstat"
             "-T"
             "depotFile,haveRev,action"
             file)
       p4-gutter-p4-timeout
       generation
       (lambda (process exit-code stdout stderr timed-out)
         (p4-gutter--handle-fstat
          generation process exit-code stdout stderr timed-out)))))))

(defun p4-gutter--update-now ()
  "Update markers using the current cached file state."
  (setq p4-gutter--update-timer nil)
  (when p4-gutter-mode
    (if p4-gutter--busy-p
        (setq p4-gutter--dirty-during-request-p t)
      (pcase p4-gutter--file-state
        ('tracked
         (let ((generation (p4-gutter--begin-request)))
           (p4-gutter--start-local-diff generation)))
        ('added
         (p4-gutter--apply-markers
          (p4-gutter--whole-file-markers 'added)))
        ('moved
         (p4-gutter--apply-markers
          (p4-gutter--whole-file-markers 'changed)))
        ('deleted
         (p4-gutter--apply-markers (list (cons 1 'removed))))))))

(defun p4-gutter--schedule-update (&rest _ignored)
  "Schedule an asynchronous gutter update after the current edit."
  (when p4-gutter-mode
    (if p4-gutter--busy-p
        (setq p4-gutter--dirty-during-request-p t)
      (p4-gutter--cancel-update-timer)
      (let ((buffer (current-buffer)))
        (setq p4-gutter--update-timer
              (run-with-idle-timer
               p4-gutter-update-delay nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (p4-gutter--update-now))))))))))

(defun p4-gutter--retry-after-command ()
  "Retry a failed request when its delay has elapsed."
  (when (and p4-gutter-mode
             (eq p4-gutter--status 'error)
             p4-gutter--retry-at
             (not p4-gutter--busy-p)
             (>= (float-time) p4-gutter--retry-at))
    (let ((kind p4-gutter--retry-kind))
      (setq p4-gutter--retry-at nil)
      (if (and (eq kind 'diff)
               p4-gutter--baseline-ready-p)
          (let ((generation (p4-gutter--begin-request)))
            (p4-gutter--start-local-diff generation))
        (p4-gutter-refresh)))))

(defun p4-gutter--after-revert ()
  "Refresh Perforce state after reverting the current buffer."
  (when p4-gutter-mode
    (p4-gutter-refresh)))

;;;###autoload
(define-minor-mode p4-gutter-mode
  "Show asynchronous Perforce change indicators in the left fringe."
  :lighter (:eval (p4-gutter--mode-line))
  :group 'p4-gutter
  (if p4-gutter-mode
      (if (or (not buffer-file-name)
              (file-remote-p buffer-file-name))
          (progn
            (setq p4-gutter-mode nil)
            (message "p4-gutter: current buffer is not a local file"))
        (add-hook 'after-change-functions #'p4-gutter--schedule-update nil t)
        (add-hook 'after-save-hook #'p4-gutter--schedule-update nil t)
        (add-hook 'after-revert-hook #'p4-gutter--after-revert nil t)
        (add-hook 'post-command-hook #'p4-gutter--retry-after-command nil t)
        (add-hook 'kill-buffer-hook #'p4-gutter--cleanup nil t)
        (add-hook 'change-major-mode-hook #'p4-gutter--cleanup nil t)
        (p4-gutter-refresh))
    (remove-hook 'after-change-functions #'p4-gutter--schedule-update t)
    (remove-hook 'after-save-hook #'p4-gutter--schedule-update t)
    (remove-hook 'after-revert-hook #'p4-gutter--after-revert t)
    (remove-hook 'post-command-hook #'p4-gutter--retry-after-command t)
    (remove-hook 'kill-buffer-hook #'p4-gutter--cleanup t)
    (remove-hook 'change-major-mode-hook #'p4-gutter--cleanup t)
    (p4-gutter--cleanup)
    (force-mode-line-update)))

(defun p4-gutter--turn-on ()
  "Enable `p4-gutter-mode' in eligible TnT project buffers."
  (when (p4-gutter--eligible-buffer-p)
    (p4-gutter-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-p4-gutter-mode
  p4-gutter-mode
  p4-gutter--turn-on
  :group 'p4-gutter)

(provide 'p4-gutter)

;;; p4-gutter.el ends here
