;;; ddf-ts-mode.el --- Tree-sitter highlighting for DDF -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Major mode for Frostbite Data Definition Format.  The bundled grammar is
;; compiled locally on first use; no network access or tree-sitter CLI is needed.
;; Run `ddf-ts-mode-install-grammar' to rebuild it after grammar changes.
;; Verbatim C++ and legacy C# blocks are highlighted as opaque code, not DDF.

;;; Code:

(require 'seq)
(require 'treesit)

(defgroup ddf-ts-mode nil
  "Editing Data Definition Format files."
  :group 'languages)

(defcustom ddf-ts-mode-indent-offset 4
  "Number of spaces used for DDF indentation."
  :type 'integer
  :safe #'natnump
  :group 'ddf-ts-mode)

(defconst ddf-ts-mode--grammar-directory
  (expand-file-name "tree-sitter-ddf"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the bundled DDF grammar.")

(defvar ddf-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\r "> b" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    table)
  "Syntax table for `ddf-ts-mode'.")

;;;###autoload
(defun ddf-ts-mode-install-grammar ()
  "Compile and install the bundled DDF tree-sitter grammar.
Requires a C compiler (cc, gcc or clang) on `exec-path'.  Build failures
are reported in the buffer *DDF grammar build*.  Restart Emacs after
rebuilding an already loaded grammar."
  (interactive)
  (unless (treesit-available-p)
    (user-error "DDF mode requires Emacs with tree-sitter support"))
  (let* ((compiler (seq-some #'executable-find
                            (if (eq system-type 'windows-nt)
                                '("clang" "gcc" "cc")
                              '("cc" "gcc" "clang"))))
         (source (expand-file-name "src" ddf-ts-mode--grammar-directory))
         (suffix (car dynamic-library-suffixes))
         (directory (locate-user-emacs-file "tree-sitter"))
         (library (expand-file-name (concat "libtree-sitter-ddf" suffix) directory))
         (output (get-buffer-create "*DDF grammar build*")))
    (unless compiler
      (user-error "Install cc, gcc or clang, then run M-x ddf-ts-mode-install-grammar"))
    (unless (file-exists-p (expand-file-name "parser.c" source))
      (user-error "Bundled DDF parser is missing from %s" source))
    (let* ((workdir (make-temp-file "ddf-grammar-" t))
           (built-library (expand-file-name (file-name-nondirectory library) workdir)))
      (unwind-protect
          (progn
            (with-current-buffer output
              (erase-buffer))
            (message "Compiling the bundled DDF grammar...")
            (let ((status
                   (apply #'call-process compiler nil (list output t) nil
                          (append '("-O2" "-shared")
                                  (unless (eq system-type 'windows-nt) '("-fPIC"))
                                  (list "-I" source
                                        (expand-file-name "parser.c" source)
                                        "-o" built-library)))))
              (unless (eql status 0)
                (display-buffer output)
                (user-error "DDF grammar compilation failed (%s); see *DDF grammar build*"
                            status)))
            (make-directory directory t)
            ;; Windows cannot overwrite a grammar DLL already loaded by Emacs.
            (when (file-exists-p library)
              (rename-file library (concat library ".old") t))
            (copy-file built-library library t)
            (unless (treesit-language-available-p 'ddf)
              (user-error "Compiled DDF grammar is not loadable: %S"
                          (treesit-language-available-p 'ddf t)))
            (message "Installed DDF grammar; restart Emacs if replacing a loaded grammar"))
        (delete-directory workdir t)))))

(defun ddf-ts-mode--font-lock-settings ()
  "Return the tree-sitter font-lock rules for DDF."
  (treesit-font-lock-rules
   :language 'ddf :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'ddf :feature 'string
   '((string_literal) @font-lock-string-face)

   :language 'ddf :feature 'preprocessor
   '((preprocessor_directive) @font-lock-preprocessor-face
     (attribute name: (qualified_identifier) @font-lock-preprocessor-face))

   :language 'ddf :feature 'native
   '((cpp_block) @font-lock-doc-face
     (csharp_block) @font-lock-doc-face)

   :language 'ddf :feature 'keyword
   '((modifier) @font-lock-keyword-face
     ["import" "module" "namespace" "class" "struct" "nativestruct"
      "interface" "enum" "event" "message" "entity" "component"
      "extend" "extendable" "as" "function" "functiondecl" "delegate" "inline"
      "hash" "data" "client" "server"] @font-lock-keyword-face)

   :language 'ddf :feature 'type
   '((type_reference) @font-lock-type-face
     (type_declaration name: (qualified_identifier) @font-lock-type-face)
     (enum_declaration name: (qualified_identifier) @font-lock-type-face)
     (extension_declaration name: (qualified_identifier) @font-lock-type-face)
     (extension_declaration extent: (qualified_identifier) @font-lock-type-face))

   :language 'ddf :feature 'definition
   '((field_declaration name: (identifier) @font-lock-variable-name-face)
     (event_field_declaration name: (identifier) @font-lock-variable-name-face)
     (parameter name: (identifier) @font-lock-variable-name-face)
     (function_declaration name: (qualified_identifier) @font-lock-function-name-face)
     (instance_declaration name: (_) @font-lock-variable-name-face)
     (enum_member name: (identifier) @font-lock-constant-face)
     (hash_declaration name: (identifier) @font-lock-constant-face)
     (section_label name: (identifier) @font-lock-constant-face)
     (module_declaration name: (qualified_identifier) @font-lock-constant-face)
     (namespace_declaration name: (qualified_identifier) @font-lock-constant-face))

   :language 'ddf :feature 'constant
   '((boolean_literal) @font-lock-constant-face
     (null_literal) @font-lock-constant-face
     (number_literal) @font-lock-number-face
     (field_declaration value: (qualified_identifier) @font-lock-constant-face)
     (event_field_declaration value: (qualified_identifier) @font-lock-constant-face)
     (named_argument value: (qualified_identifier) @font-lock-constant-face)
     (enum_member value: (qualified_identifier) @font-lock-constant-face)
     (argument_list (qualified_identifier) @font-lock-constant-face)
     (array_initializer (qualified_identifier) @font-lock-constant-face)
     (object_initializer (qualified_identifier) @font-lock-constant-face)
     (parenthesized_expression (qualified_identifier) @font-lock-constant-face)
     (unary_expression operand: (qualified_identifier) @font-lock-constant-face)
     (binary_expression (qualified_identifier) @font-lock-constant-face)
     (asset_path) @font-lock-constant-face)

   :language 'ddf :feature 'property
   '((named_argument name: (qualified_identifier) @font-lock-property-use-face)
     (call_expression function: (qualified_identifier) @font-lock-function-call-face))

   :language 'ddf :feature 'operator
   '((binary_expression operator: _ @font-lock-operator-face)
     (unary_expression operator: _ @font-lock-operator-face)
     "=" @font-lock-operator-face)

   :language 'ddf :feature 'bracket
   '(["{" "}" "[" "]" "(" ")"] @font-lock-bracket-face)))

;;;###autoload
(define-derived-mode ddf-ts-mode prog-mode "DDF"
  "Major mode for DDF declarations, using tree-sitter highlighting.
Compile the bundled grammar on first use if it is not installed."
  :syntax-table ddf-ts-mode-syntax-table
  (unless (treesit-available-p)
    (user-error "DDF mode requires Emacs with tree-sitter support"))
  (unless (treesit-language-available-p 'ddf)
    (ddf-ts-mode-install-grammar))
  (treesit-parser-create 'ddf)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local treesit-font-lock-settings (ddf-ts-mode--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '((comment string) (keyword type preprocessor native)
                (definition constant property) (operator bracket)))
  (setq-local treesit-simple-indent-rules
              `((ddf
                 ((node-is "cpp_block") no-indent 0)
                 ((node-is "csharp_block") no-indent 0)
                 ((node-is "}") parent-bol 0)
                 ((node-is "]") parent-bol 0)
                 ((node-is ")") parent-bol 0)
                 ((parent-is "source_file") column-0 0)
                 ((parent-is "type_body") parent-bol ,ddf-ts-mode-indent-offset)
                 ((parent-is "enum_body") parent-bol ,ddf-ts-mode-indent-offset)
                 ((parent-is "object_initializer") parent-bol ,ddf-ts-mode-indent-offset)
                 ((parent-is "array_initializer") parent-bol ,ddf-ts-mode-indent-offset)
                 ((parent-is "parameter_list") parent-bol ,ddf-ts-mode-indent-offset)
                 (no-node parent-bol 0))))
  (setq-local treesit-simple-imenu-settings
              '(("Types" "\\`\\(type\\|enum\\)_declaration\\'" nil nil)
                ("Extensions" "\\`extension_declaration\\'" nil nil)
                ("Functions" "\\`function_declaration\\'" nil nil)
                ("Instances" "\\`instance_declaration\\'" nil nil)))
  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ddf\\'" . ddf-ts-mode))

(provide 'ddf-ts-mode)
;;; ddf-ts-mode.el ends here
