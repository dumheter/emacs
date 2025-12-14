;;; treesit-utils.el --- Tree-sitter utilities for C++ symbol browsing -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides `my-consult-cpp-symbols' to scan a C++ project using tree-sitter
;; and browse symbols interactively with consult.

;;; Code:

(require 'treesit)
(require 'consult)
(require 'projectile)

;; --- Configuration ---

(defvar my-cpp-symbols-cache (make-hash-table :test 'equal)
  "Cache for C++ symbols, keyed by project root.")

(defvar my-cpp-symbols-cache-duration 600
  "Cache validity duration in seconds (default 10 minutes).")

(defvar my-cpp-symbols-search-subdir "Code/DICE/Extensions/BattlefieldOnline"
  "Subdirectory within project to scan for C++ files.")

(defvar my-cpp-symbols--lookup-table nil
  "Hash table mapping display strings to symbol plists for current session.")

(defun my-cpp-symbols--search-directory (project-root)
  "Return the directory to search for C++ files in PROJECT-ROOT.
If PROJECT-ROOT is named \"TnT\", use `my-cpp-symbols-search-subdir'.
Otherwise, search in PROJECT-ROOT itself."
  (let ((project-name (file-name-nondirectory
                       (directory-file-name project-root))))
    (if (string= project-name "TnT")
        (expand-file-name my-cpp-symbols-search-subdir project-root)
      (expand-file-name "." project-root))))

;; --- Tree-sitter Query ---

(defvar my-cpp-symbols--query nil
  "Compiled tree-sitter query for C++ symbols.")

(defun my-cpp-symbols--ensure-query ()
  "Ensure the tree-sitter query is compiled."
  (unless my-cpp-symbols--query
    (setq my-cpp-symbols--query
          (treesit-query-compile
           'cpp
           '((function_definition
              declarator: (function_declarator
                           declarator: (_) @func))
             (class_specifier
              name: (type_identifier) @class)
             (struct_specifier
              name: (type_identifier) @struct)
             (enum_specifier
              name: (type_identifier) @enum)
             (alias_declaration
              name: (type_identifier) @alias)
             (type_definition
              declarator: (type_identifier) @typedef))))))

;; --- Symbol Extraction ---

(defun my-cpp-symbols--node-name (node)
  "Extract the symbol name from NODE, handling qualified identifiers."
  (let ((type (treesit-node-type node)))
    (cond
     ;; Qualified identifier: get full text (e.g., "Foo::bar")
     ((string= type "qualified_identifier")
      (treesit-node-text node t))
     ;; Destructor: prefix with ~
     ((string= type "destructor_name")
      (concat "~" (treesit-node-text
                   (treesit-node-child node 0) t)))
     ;; Template function: just get the name part
     ((string= type "template_function")
      (treesit-node-text
       (treesit-node-child-by-field-name node "name") t))
     ;; Simple identifier or other
     (t
      (treesit-node-text node t)))))

(defun my-cpp-symbols--capture-to-type (capture-name)
  "Convert CAPTURE-NAME to a symbol type string."
  (pcase capture-name
    ('func "function")
    ('class "class")
    ('struct "struct")
    ('enum "enum")
    ('alias "alias")
    ('typedef "typedef")
    (_ "unknown")))

(defun my-cpp-symbols--parse-file (file)
  "Parse FILE and return a list of symbol plists."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (let ((lang (treesit-language-at (point-min))))
          ;; Ensure we have C++ tree-sitter
          (unless lang
            (setq-local treesit-language-source-alist
                        '((cpp . nil)))
            (treesit-parser-create 'cpp))
          (let* ((root (treesit-buffer-root-node 'cpp))
                 (captures (treesit-query-capture
                            root my-cpp-symbols--query)))
            (mapcar
             (lambda (capture)
               (let* ((name-sym (car capture))
                      (node (cdr capture))
                      (sym-name (my-cpp-symbols--node-name node))
                      (sym-type (my-cpp-symbols--capture-to-type name-sym))
                      (line (line-number-at-pos
                             (treesit-node-start node))))
                 (list :name sym-name
                       :type sym-type
                       :file file
                       :line line)))
             captures))))
    (error nil)))

;; --- Project Scanning ---

(defun my-cpp-symbols--scan-project (project-root)
  "Scan C++ files in PROJECT-ROOT and return list of symbols."
  (my-cpp-symbols--ensure-query)
  (let* ((search-dir (my-cpp-symbols--search-directory project-root))
         (files (when (file-directory-p search-dir)
                  (directory-files-recursively
                   search-dir
                   "\\.\\(cpp\\|h\\|hpp\\|cc\\|cxx\\)$")))
         (total (length files))
         (count 0)
         (symbols '()))
    (if (null files)
        (progn
          (message "No C++ files found in %s" search-dir)
          nil)
      (dolist (file files)
        (setq count (1+ count))
        (message "Scanning C++ symbols... %d/%d: %s"
                 count total (file-name-nondirectory file))
        (let ((file-symbols (my-cpp-symbols--parse-file file)))
          (setq symbols (nconc symbols file-symbols))))
      (message "Scanned %d symbols from %d files" (length symbols) total)
      symbols)))

;; --- Cache Management ---

(defun my-cpp-symbols--cache-valid-p (cache-entry)
  "Return non-nil if CACHE-ENTRY is still valid."
  (and cache-entry
       (let ((timestamp (plist-get cache-entry :timestamp)))
         (< (- (float-time) timestamp)
            my-cpp-symbols-cache-duration))))

(defun my-cpp-symbols--get-symbols (project-root &optional force-refresh)
  "Get symbols for PROJECT-ROOT, using cache unless FORCE-REFRESH."
  (let ((cache-entry (gethash project-root my-cpp-symbols-cache)))
    (if (and (not force-refresh)
             (my-cpp-symbols--cache-valid-p cache-entry))
        (plist-get cache-entry :symbols)
      ;; Refresh cache
      (let ((symbols (my-cpp-symbols--scan-project project-root)))
        (puthash project-root
                 (list :timestamp (float-time)
                       :symbols symbols)
                 my-cpp-symbols-cache)
        symbols))))

(defun my-cpp-symbols-clear-cache ()
  "Clear the C++ symbols cache."
  (interactive)
  (clrhash my-cpp-symbols-cache)
  (message "C++ symbols cache cleared"))

;; --- Consult Integration ---

(defun my-cpp-symbols--make-lookup-table (symbols)
  "Create a hash table mapping display strings to SYMBOLS."
  (let ((table (make-hash-table :test 'equal :size (length symbols))))
    (dolist (sym symbols)
      (let* ((name (plist-get sym :name))
             (type (plist-get sym :type))
             (file (plist-get sym :file))
             (line (plist-get sym :line))
             (filename (file-name-nondirectory file))
             (display (format "%-50s %-10s %s:%d"
                              name type filename line)))
        (puthash display sym table)))
    table))

(defun my-cpp-symbols--get-symbol (candidate)
  "Get symbol plist for CANDIDATE string from lookup table."
  (when (and candidate my-cpp-symbols--lookup-table)
    (gethash candidate my-cpp-symbols--lookup-table)))

(defun my-cpp-symbols--goto-symbol (sym)
  "Navigate to SYM's file and line."
  (when sym
    (let ((file (plist-get sym :file))
          (line (plist-get sym :line)))
      (when (and file line (file-exists-p file))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))
        (recenter)))))

(defvar my-cpp-symbols-preview-delay 0.5
  "Delay in seconds before previewing a symbol.")

(defvar my-cpp-symbols--preview-timer nil
  "Timer for delayed preview.")

(defun my-cpp-symbols--state ()
  "State function for consult preview with debounced file loading."
  (let ((open-buffers (buffer-list))
        (preview-window (selected-window)))
    (lambda (action cand)
      ;; Cancel any pending preview timer
      (when my-cpp-symbols--preview-timer
        (cancel-timer my-cpp-symbols--preview-timer)
        (setq my-cpp-symbols--preview-timer nil))
      (pcase action
        ('preview
         (when-let* ((sym (my-cpp-symbols--get-symbol cand))
                     (file (plist-get sym :file))
                     (line (plist-get sym :line)))
           (when (file-exists-p file)
             ;; Check if buffer already exists (instant preview)
             (let ((existing-buf (get-file-buffer file)))
               (if existing-buf
                   ;; Buffer already open - preview immediately
                   (progn
                     (with-current-buffer existing-buf
                       (goto-char (point-min))
                       (forward-line (1- line)))
                     (set-window-buffer preview-window existing-buf))
                 ;; Buffer not open - delay loading
                 (setq my-cpp-symbols--preview-timer
                       (run-with-idle-timer
                        my-cpp-symbols-preview-delay nil
                        (lambda ()
                          (when (and (window-live-p preview-window)
                                     (file-exists-p file))
                            (let ((buf (find-file-noselect file)))
                              (with-current-buffer buf
                                (goto-char (point-min))
                                (forward-line (1- line)))
                              (set-window-buffer preview-window buf)))))))))))
        ('exit
         ;; Close buffers we opened for preview
         (dolist (buf (buffer-list))
           (unless (memq buf open-buffers)
             (when (buffer-file-name buf)
               (kill-buffer buf)))))))))

;; --- Entry Point ---

(defun my-find-symbols (&optional force-refresh)
  "Browse C++ symbols in the current project using consult.
With prefix arg FORCE-REFRESH, rescan the project ignoring cache."
  (interactive "P")
  (let* ((project-root (or (projectile-project-root)
                           (user-error "Not in a projectile project")))
         (symbols (my-cpp-symbols--get-symbols project-root force-refresh)))
    (if (null symbols)
        (user-error "No symbols found")
      ;; Build lookup table and candidate list
      (setq my-cpp-symbols--lookup-table
            (my-cpp-symbols--make-lookup-table symbols))
      (let* ((candidates (hash-table-keys my-cpp-symbols--lookup-table))
             (selected (consult--read
                        candidates
                        :prompt "C++ Symbol: "
                        :category 'cpp-symbol
                        :state (my-cpp-symbols--state)
                        :require-match t
                        :sort nil)))
        (when selected
          (my-cpp-symbols--goto-symbol
           (my-cpp-symbols--get-symbol selected)))))))

(provide 'treesit-utils)
;;; treesit-utils.el ends here
