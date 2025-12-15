;;; treesit-utils.el --- Tree-sitter utilities for C++ symbol browsing -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides `find-symbols' to scan a C++ project using tree-sitter
;; and browse symbols interactively with consult.

;;; Code:

(require 'treesit)
(require 'consult)
(require 'projectile)

;; --- Configuration ---

(defvar treesit-utils-symbols-cache (make-hash-table :test 'equal)
  "Cache for C++ symbols, keyed by project root.")

(defvar treesit-utils-symbols-cache-duration (* 30 24 60 60)
  "Cache validity duration in seconds (default 30 days).")

(defvar treesit-utils-symbols-cache-dir ".cache"
  "Subdirectory within project root for cache files.")

(defvar treesit-utils-symbols-cache-filename ".emacs-symbols"
  "Filename for the serialized symbol cache.")

(defvar treesit-utils-symbols-search-subdir "Code/DICE/Extensions/BattlefieldOnline"
  "Subdirectory within project to scan for C++ files.")

(defvar treesit-utils-symbols--lookup-table nil
  "Hash table mapping display strings to symbol plists for current session.")

(defun treesit-utils-symbols--search-directory (project-root)
  "Return the directory to search for C++ files in PROJECT-ROOT.
If PROJECT-ROOT is named \"TnT\", use `treesit-utils-symbols-search-subdir'.
Otherwise, search in PROJECT-ROOT itself."
  (let ((project-name (file-name-nondirectory
                       (directory-file-name project-root))))
    (if (string= project-name "TnT")
        (expand-file-name treesit-utils-symbols-search-subdir project-root)
      (expand-file-name "." project-root))))

;; --- Tree-sitter Query ---

(defvar treesit-utils-symbols--query nil
  "Compiled tree-sitter query for C++ symbols.")

(defun treesit-utils-symbols--ensure-query ()
  "Ensure the tree-sitter query is compiled."
  (unless treesit-utils-symbols--query
    (setq treesit-utils-symbols--query
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

(defun treesit-utils-symbols--node-name (node)
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

(defun treesit-utils-symbols--capture-to-type (capture-name)
  "Convert CAPTURE-NAME to a symbol type string."
  (pcase capture-name
    ('func "function")
    ('class "class")
    ('struct "struct")
    ('enum "enum")
    ('alias "alias")
    ('typedef "typedef")
    (_ "unknown")))

(defun treesit-utils-symbols--parse-file (file)
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
                            root treesit-utils-symbols--query)))
            (mapcar
             (lambda (capture)
               (let* ((name-sym (car capture))
                      (node (cdr capture))
                      (sym-name (treesit-utils-symbols--node-name node))
                      (sym-type (treesit-utils-symbols--capture-to-type name-sym))
                      (line (line-number-at-pos
                             (treesit-node-start node))))
                 (list :name sym-name
                       :type sym-type
                       :file file
                       :line line)))
             captures))))
    (error nil)))

;; --- Project Scanning ---

(defun treesit-utils-symbols--scan-project (project-root)
  "Scan C++ files in PROJECT-ROOT and return list of symbols."
  (treesit-utils-symbols--ensure-query)
  (let* ((search-dir (treesit-utils-symbols--search-directory project-root))
         (files (when (file-directory-p search-dir)
                  (directory-files-recursively
                   search-dir
                   "\\.\\(cpp\\|h\\|hpp\\|cc\\|cxx\\|c\\)$")))
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
        (let ((file-symbols (treesit-utils-symbols--parse-file file)))
          (setq symbols (nconc symbols file-symbols))))
      (message "Scanned %d symbols from %d files" (length symbols) total)
      symbols)))

;; --- Cache Management ---

(defun treesit-utils-symbols--cache-file-path (project-root)
  "Return the path to the cache file for PROJECT-ROOT."
  (expand-file-name treesit-utils-symbols-cache-filename
                    (expand-file-name treesit-utils-symbols-cache-dir project-root)))

(defun treesit-utils-symbols--save-cache (project-root cache-entry)
  "Save CACHE-ENTRY to disk for PROJECT-ROOT."
  (let* ((cache-dir (expand-file-name treesit-utils-symbols-cache-dir project-root))
         (cache-file (treesit-utils-symbols--cache-file-path project-root)))
    (condition-case err
        (progn
          (unless (file-directory-p cache-dir)
            (make-directory cache-dir t))
          (with-temp-file cache-file
            (let ((print-length nil)
                  (print-level nil))
              (prin1 cache-entry (current-buffer))))
          (message "Saved symbol cache to %s" cache-file))
      (error
       (message "Failed to save symbol cache: %s" (error-message-string err))))))

(defun treesit-utils-symbols--load-cache (project-root)
  "Load cache entry from disk for PROJECT-ROOT.
Returns the cache plist or nil if not found or invalid."
  (let ((cache-file (treesit-utils-symbols--cache-file-path project-root)))
    (when (file-exists-p cache-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents cache-file)
            (read (current-buffer)))
        (error nil)))))

(defun treesit-utils-symbols--cache-valid-p (cache-entry)
  "Return non-nil if CACHE-ENTRY is still valid."
  (and cache-entry
       (let ((timestamp (plist-get cache-entry :timestamp)))
         (< (- (float-time) timestamp)
            treesit-utils-symbols-cache-duration))))

(defun treesit-utils-symbols--get-symbols (project-root &optional force-refresh)
  "Get symbols for PROJECT-ROOT, using cache unless FORCE-REFRESH.
Checks in-memory cache first, then disk cache, then scans project."
  (let ((cache-entry (gethash project-root treesit-utils-symbols-cache)))
    ;; 1. Check in-memory cache
    (if (and (not force-refresh)
             (treesit-utils-symbols--cache-valid-p cache-entry))
        (plist-get cache-entry :symbols)
      ;; 2. Check disk cache
      (let ((disk-cache (and (not force-refresh)
                             (treesit-utils-symbols--load-cache project-root))))
        (if (treesit-utils-symbols--cache-valid-p disk-cache)
            (progn
              ;; Load disk cache into memory
              (puthash project-root disk-cache treesit-utils-symbols-cache)
              (message "Loaded symbol cache from disk")
              (plist-get disk-cache :symbols))
          ;; 3. Scan project and save to both caches
          (let* ((symbols (treesit-utils-symbols--scan-project project-root))
                 (new-cache-entry (list :timestamp (float-time)
                                        :symbols symbols)))
            (puthash project-root new-cache-entry treesit-utils-symbols-cache)
            (treesit-utils-symbols--save-cache project-root new-cache-entry)
            symbols))))))

(defun treesit-utils-symbols-clear-cache ()
  "Clear the C++ symbols cache (both in-memory and disk)."
  (interactive)
  (let ((project-root (projectile-project-root)))
    ;; Clear in-memory cache
    (clrhash treesit-utils-symbols-cache)
    ;; Delete disk cache file for current project
    (when project-root
      (let ((cache-file (treesit-utils-symbols--cache-file-path project-root)))
        (when (file-exists-p cache-file)
          (delete-file cache-file)
          (message "Deleted disk cache: %s" cache-file)))))
  (message "C++ symbols cache cleared"))

;; --- Consult Integration ---

(defun treesit-utils-symbols--make-lookup-table (symbols)
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

(defun treesit-utils-symbols--get-symbol (candidate)
  "Get symbol plist for CANDIDATE string from lookup table."
  (when (and candidate treesit-utils-symbols--lookup-table)
    (gethash candidate treesit-utils-symbols--lookup-table)))

(defun treesit-utils-symbols--goto-symbol (sym)
  "Navigate to SYM's file and line."
  (when sym
    (let ((file (plist-get sym :file))
          (line (plist-get sym :line)))
      (when (and file line (file-exists-p file))
        (xref-push-marker-stack)
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))
        (recenter)))))

(defvar treesit-utils-symbols-preview-delay 0.5
  "Delay in seconds before previewing a symbol.")

(defvar treesit-utils-symbols--preview-timer nil
  "Timer for delayed preview.")

(defun treesit-utils-symbols--state ()
  "State function for consult preview with debounced file loading."
  (let ((open-buffers (buffer-list))
        (preview-window (selected-window)))
    (lambda (action cand)
      ;; Cancel any pending preview timer
      (when treesit-utils-symbols--preview-timer
        (cancel-timer treesit-utils-symbols--preview-timer)
        (setq treesit-utils-symbols--preview-timer nil))
      (pcase action
        ('preview
         (when-let* ((sym (treesit-utils-symbols--get-symbol cand))
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
                 (setq treesit-utils-symbols--preview-timer
                       (run-with-idle-timer
                        treesit-utils-symbols-preview-delay nil
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

(defun treesit-utils-find-symbols (&optional force-refresh)
  "Browse C++ symbols in the current project using consult.
With prefix arg FORCE-REFRESH, rescan the project ignoring cache."
  (interactive "P")
  (let* ((project-root (or (projectile-project-root)
                           (user-error "Not in a projectile project")))
         (symbols (treesit-utils-symbols--get-symbols project-root force-refresh)))
    (if (null symbols)
        (user-error "No symbols found")
      ;; Build lookup table and candidate list
      (setq treesit-utils-symbols--lookup-table
            (treesit-utils-symbols--make-lookup-table symbols))
      (let* ((candidates (hash-table-keys treesit-utils-symbols--lookup-table))
             (selected (consult--read
                        candidates
                        :prompt "C++ Symbol: "
                        :category 'cpp-symbol
                        :state (treesit-utils-symbols--state)
                        :require-match t
                        :sort nil)))
        (when selected
          (treesit-utils-symbols--goto-symbol
           (treesit-utils-symbols--get-symbol selected)))))))

(provide 'treesit-utils)
;;; treesit-utils.el ends here
