;; # Readme
;;
;; ## Install
;; Do this first time you use this config:
;;
;; ### lsp-bridge
;; > cd ~ && mkdir lsp-bridge && cd lsp-bridge
;; > git init
;; (http) > git remote add origin https://github.com/manateelazycat/lsp-bridge.git
;; (ssh)  > git remote add origin git@github.com:manateelazycat/lsp-bridge.git
;; 
;; ### clangd big project
;; 1. Locate your lsp-bridge install.
;; 2. In the langserver folder, check for clangd.json.
;; 3. insert these at the end:
;;   "--completion-style=detailed",
;;   "--background-index=false"
;;
;; ### Windows Config
;; #### Open files in same window
;; Accociate your files with emacsclientw.exe. This will open a new window.
;; To make it open in the current window, open regedit:
;;   > HKEY_CLASSES_ROOT\Applications\emacsclientw.exe\shell\open\command
;; Make sure it says (add --no-wait):
;;   > "C:\Path\To\emacsclientw.exe" --no-wait "%1"
;;
;; #### Hunspell
;; Install hunspell.
;;   > choco install hunspell.portable
;; Copy over en_US.{aff,dic} to C:/Hunspell
;,
;; ## Help
;;
;; ### Not loading this file
;; If windows doesnt use .emacs.d default folder, look for what init
;; file was used by calling
;;   > `describe-variable` user-init-file
;; Then load this file from there with
;;   > (load "~/.emacs.d/init.el)
;;
;; ### lsp-bridge doesnt work.
;; Search for `App Execution Aliases` and turn off python.

;; TODO
;; * ai copilot


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package warnings
  :ensure nil
  :config
  (add-to-list 'warning-suppress-types '(undo discard-info))
  )

(use-package emacs
  :ensure nil ;; buildin package
  :bind (
	 ("C-a" . smarter-begining-of-line)
	 ("C-+" . text-scale-increase)
	 ("C--" . text-scale-decrease)
	 ("M-p" . backward-paragraph)
	 ("M-n" . forward-paragraph)
	 ("C-S-<up>" . move-line-up)
	 ("C-S-<down>" . move-line-down)
	 ("M-C-SPC" . mark-sexp-at-point)
	 ("C-c f" . recentf)
	 )
  :custom

  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x whihc do not work in current mode. Vertico
  ;; commands are hidden in normal buffers. This setting is usful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do now allow the cursor in the minibuffer promp
  (minibuffer-promt-properties
   '(read-only t cursor-intagible t face minibuffer-prompt))

  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq inhibit-startup-screen t)
  (setq ring-bell-function 'ignore)
  (setq make-backup-files nil) ;; Disable backup files (file~)
  (setq auto-save-default nil) ;; Disable auto-save files (#file#)
  (setq create-lockfiles nil) ;; Disable lockfiles (.#file)
  (setq backup-inhibited t)
  (setq-default intent-tabs-mode t)
  (setq-default tab-width 4)
  (setq blink-cursor-mode nil)

  ;; UTF-8 please
  (setq locale-coding-system 'utf-8) ; pretty
  (set-terminal-coding-system 'utf-8) ; pretty
  (set-keyboard-coding-system 'utf-8) ; pretty
  (set-selection-coding-system 'utf-8) ; please
  (prefer-coding-system 'utf-8) ; with sugar on top

  ;; Font
  (cond
   ((string-equal system-type "gnu/linux")
	(progn
      (set-face-attribute 'default nil :height 100))))

  (cond
   ((string-equal system-type "windows-nt")
	(progn
      (add-to-list 'default-frame-alist '(font . "Cascadia Mono PL-9"))
      (set-face-attribute 'default t
						  :font "Cascadia Mono PL-9"
						  ))))
  
  (cond
   ((string-equal system-type "windows-nt")
	(progn
	  (set-face-attribute 'fixed-pitch nil :font "Cascadia Mono PL-9")))
   )

  ;; Experimental for linux, is this right?
  (cond
   ((string-equal system-type "gnu/linux")
	(progn
      (set-face-attribute 'fixed-pitch nil :height 100))))

  ;; custom stuff in custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
	(load custom-file)
	)

  (setq scroll-conservatively 101) ;; Only scroll one step once cursor leaves window.
  (setq frame-title-format
		'("Emacs @ " (:eval (if (buffer-file-name)
							 (abbreviate-file-name (buffer-file-name))
							 (buffer-name)))))

  (global-hl-line-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (delete-selection-mode 1) ;; Deletion commands work on regions.
  )

(use-package recentf
  :ensure nil ; built in
  :hook (after-init . recentf-mode)
  :init
  (run-at-time nil (* 10 60) 'recentf-save-list) ;; save even if emacs crashes
  :config
  (setq recentf-max-saved-items 5000) ;; keep many things in recentf
  (setq recentf-save-file "~/.emacs.d/recentf")

  ;; Convert slashes to OS-native format before saving the recentf list.
  (defun my-recentf-convert-slashes-for-saving ()
    "Convert file paths in `recentf-list` to native format for Windows."
    (when (string-equal system-type "windows-nt")
      (setq recentf-list
            (mapcar (lambda (file) (replace-regexp-in-string "/" "\\\\" file t t))
                    recentf-list))))

  (add-hook 'recentf-pre-save-hook #'my-recentf-convert-slashes-for-saving)
  )

(use-package cc-mode
  ;; Don't indent after namespace.
  :mode("\\.ixx\\'" . c++-mode)
  :bind(:map c-mode-base-map
			 ("C-c n" . my-projectile-build-project)
			 ("C-c l" . my-projectile-configure-project))
  :hook (c++-mode . (lambda ()
					  (c-set-offset 'innamespace 0)
					  (c-set-offset 'namespace-open 0)
					  (c-set-offset 'substatement-open 0)))
  )

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
	(server-start))
  )

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package windmove
  :ensure nil ;; because its builtin to emacs
  :config
  (windmove-default-keybindings))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . 'mc/mark-next-like-this)
	 ("C->" . 'mc/mark-previous-like-this)
	 ("C-c C-<" . 'mc/mark-all-like-this))
  )

;; minibuffer
(use-package vertico
  :init
  (vertico-mode)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode)
  )

;; improve minibuffer with completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (style partial-completion))))
  )

(use-package consult
  :bind(
		("C-s" . consult-line)
		("C-c h" . consult-history)
        ("C-c k" . consult-kmacro)
        ("C-c m" . consult-man)
        ("C-c i" . consult-info)
		([remap Info-search] . consult-info)
		;; C-x bindings in `ctl-x-map'
        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
        ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
		;; Custom M-# bindings for fast register access
        ("M-#" . consult-register-load)
        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
        ("C-M-#" . consult-register)
		;; Other custom bindings
        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
		;; M-g bindings in `goto-map'
        ("M-g e" . consult-compile-error)
        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
        ("M-g g" . consult-goto-line)             ;; orig. goto-line
        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
        ("M-g m" . consult-mark)
        ("M-g k" . consult-global-mark)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-imenu-multi)
		;; M-s bindings in `search-map'
		("M-s d" . consult-find)                  ;; Alternative: consult-fd
        ("M-s c" . consult-locate)
        ("M-s g" . consult-grep)
        ("M-s G" . consult-git-grep)
        ("M-s r" . consult-ripgrep)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi)
        ("M-s k" . consult-keep-lines)
        ("M-s u" . consult-focus-lines)
		;; Isearch integration
        ("M-s e" . consult-isearch-history)
        :map isearch-mode-map
        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
        ;; Minibuffer history
        :map minibuffer-local-map
        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
		)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
		 (prog-mode . flyspell-prog-mode))
  :config
  ;; Do we want aspell on linux?
  (setq ispell-program-name "hunspell") ;; Make sure to have it installed.
  (setq ispell-really-hunspell t) ;; Needed for hunspell.
  (setq ispell-dictionary "en_US")
  ;; Add hunspell search paths
  (setq ispell-hunspell-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq flyspell-issue-message-flag nil) ;; Dont send a message for every issue.
  ;; Not sure what this part does:
  ;;(with-eval-after-load 'consult
	;;(define-key consult-mode-map [remap ispell-word] #'consult-flyspell))
  )

(use-package consult-flyspell
  :ensure t
  :after (consult flyspell)
  ;;:bind (:map flyspell-mode-map
			  ;;("C-." . consult-flyspell))
  ;;To correct word directly with `flyspell-correct-word'.
  ;;(setq consult-flyspell-select-function 'flyspell-correct-at-point)

  ;;To correct word directly with `flyspell-correct-word' and jump back to `consult-flyspell'.
  ;;(setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell)))
  )

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching 'persistent)
  (setq projectile-cache-file "~/.emacs.d/projectile.cache")
  (setq projectile-indexing-method 'alien)
  ;;(setq projectile-sort-order 'recently-active)
  ;;(setq projectile-generic-command "fd -e cpp -e h -e ddf -tf --color=never")
  (projectile-mode +1)

  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind(("C-c r" . my-projectile-related-file))
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do))
  )

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1)
  :bind
  (:map yas-minor-mode-map
	("C-c y i" . yas-insert-snippet)        ;; Insert a snippet
        ("C-c y n" . yas-new-snippet)           ;; Create a new snippet
        ("C-c y v" . yas-visit-snippet-file)    ;; Edit a snippet
        ("C-c y l" . yas-describe-tables))      ;; List available snippets
  )

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  )

(use-package p4
  :ensure t
  )

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  )

(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-region)
		 )
  )

(use-package clipmon
  :config
  (clipmon-mode-start)
  )

(use-package lsp-bridge
  :load-path "~/lsp-bridge"
  :demand t
  :bind
  (("M-." . my-lsp-bridge-find-def-with-xref)
	)
  :config
  (setq lsp-bridge-enable-signature-help t)
  (global-lsp-bridge-mode)
  (setq lsp-bridge-enable-inlay-hint t)
  (setq lsp-bridge-enable-hover-diagnostic t)

  (defun my-lsp-bridge-find-def-with-xref ()
	  "Use lsp-bridge-find-def but with xref mark integration."
	(interactive)
	(xref-push-marker-stack)
	(lsp-bridge-find-def))
  )

(use-package copilot-chat
  :ensure t
  )

(use-package gptel
  :ensure t
  :config
  (if (string-match "DICE" (system-name))
	  ;; then
      (setq
	   gptel-model 'gemini-2.5-pro
	   gptel-backend (gptel-make-gh-copilot "Copilot"))
	;; else
	(setq gptel-model 'lmstudio
		  gptel-backend
		  (gptel-make-openai "LM-Studio"
			:stream t
			:protocol "http"
			:host "localhost:1234"
			:models '(lmstudio))))

  ;; Optional: auto-wrap responses for readability:
  (defun my-gptel-fill-buffer ()
    (save-excursion
      (fill-region (point-min) (point-max))))
  (add-hook 'gptel-post-response-hook #'my-gptel-fill-buffer)
  )

(use-package org
  :hook (org-mode . visual-line-mode)
  )

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t)
  )

(use-package lua-mode
  :ensure t
  )

(use-package ansi-color
  :ensure nil ;; it's built-in, no need to install
  :hook (compilation-filter . my/ansi-colorize-compilation-buffer)
  :config
  (defun my/ansi-colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(use-package cmake-mode
  :ensure t
  :bind (:map cmake-mode-map
			 ("C-c n" . my-projectile-build-project)
			 ("C-c l" . my-projectile-configure-project)))

(use-package ace-jump-mode
  :ensure t
  :init
  :bind (:map global-map
			  ("C-c SPC" . ace-jump-mode)
			  ("C-x SPC" . ace-jump-mode-pop-mark))
  )

;; -------------------------------------------------------------------
;; run-exe

;; Define history variables so they are properly scoped
(defvar run-exe-last-path nil "Last used executable path for run-exe.")
(defvar run-exe-last-args nil "Last used arguments for run-exe.")

(defun run-exe (exe-path &optional args)
  "Run an executable at EXE-PATH with optional ARGS, capture stdout, stderr, and exit code, and display in a buffer."
  (interactive
   (let* ((base-dir (or run-exe-last-path default-directory))
          (path (read-file-name
                 "Path to executable: "
                 base-dir
                 run-exe-last-path
                 t))
          (arg-string (read-string
                       "Arguments (space-separated): "
                       nil ; history
                       run-exe-last-args)))
     ;; Store the values for the next run
     (setq run-exe-last-path path)
     (setq run-exe-last-args arg-string)
     ;; Return the arguments for the function
     (list path arg-string)))

  (let* (;; Split arguments only if the string is not nil or empty
         (arg-list (if (and args (not (string-blank-p args))) (split-string args " " t) nil))
         (buffer-name (concat "*run-exe: " (file-name-nondirectory exe-path) "*"))
         (output-buffer (get-buffer-create buffer-name))
         (exit-code 0))

    ;; Switch to the output buffer to prepare it
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Running: %s %s\n" exe-path (or args "")))
        (insert "--- stdout & stderr ---\n\n")))

    ;; --- KEY FIX ---
    ;; Run the process directly, sending output to our buffer.
    ;; The third argument to call-process is DESTINATION.
    ;; By providing a list `(BUFFER T)`, we instruct it to send
    ;; both stdout and stderr to our buffer.
    (setq exit-code (apply #'call-process exe-path nil `(,output-buffer t) nil arg-list))

    ;; Switch back to the output buffer to add final info and display it
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max)) ; Go to the end to append
        (insert "\n--- Process Finished ---\n")
        (insert (format "Exit code: %d\n" exit-code))
        (goto-char (point-min)) ; Go to the top for the user
        (special-mode))) ; A simple mode for output buffers

    (display-buffer output-buffer)
    (message "Executable '%s' finished with exit code: %d" (file-name-nondirectory exe-path) exit-code)))

;; -------------------------------------------------------------------
;; run-exe-async

;; Define the sentinel function first, as it will be referenced by run-exe-async
(defun run-exe-async--sentinel (process event)
  "Process sentinel for `run-exe-async'. Appends exit status to the buffer."
  (let ((buffer (process-buffer process))
        (exit-status-info nil))
    (cond
     ((string-match "\\(finished\\|exited\\|signal\\)" event)
      (setq exit-status-info
            (format "\n--- Process %s %s --- Exit code: %s\n"
                    (process-name process)
                    (replace-regexp-in-string "\n$" "" event)
                    (process-exit-status process))))
     )
    (when (and exit-status-info buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((buffer-read-only nil))
          (goto-char (point-max))
          (insert exit-status-info)
          (goto-char (point-max))
          (setq mode-line-process nil) ; Clear process from mode-line on exit
          )))))

;; Variable to store the last used arguments (list of strings)
(defvar run-exe-async--last-args nil
  "List of strings representing the last arguments used by `run-exe-async`.")

(defun run-exe-async (exe-path &optional args)
  "Run EXE-PATH asynchronously with optional ARGS (list of strings).

Capture stdout/stderr in real-time into a buffer named after
the executable (*myprogram.exe-output*). Append the exit code
upon completion.

When called INTERACTIVELY:
- Prompts for the executable path.
- Prompts for arguments, suggesting the last used arguments
  (from `run-exe-async--last-args`) as the default value.
- The arguments provided interactively are remembered for the next call.

When called PROGRAMMATICALLY:
- If ARGS is provided (non-nil list), these args are used and
  remembered as the 'last arguments' for the next interactive call.
- If ARGS is nil, the command runs with no arguments, and the
  'last arguments' suggestion is NOT updated.

Returns the process object on success, nil on failure to start."

  ;; --- Interactive Argument Handling ---
  (interactive
   (let* ((base-dir (or run-exe-last-path default-directory))
		  (path (read-file-name
				 "Path to executable: "
				 base-dir
				 run-exe-last-path
				 t))
		  (arg-string (read-string
					   "Arguments (space-separated): "
					   run-exe-last-args)))
	 (list path arg-string)))

  ;; --- Main Function Body ---

  ;; Basic check (using the provided exe-path)
  (unless (file-exists-p exe-path)
    (message "Error: File does not exist: %s" exe-path)
    (error "File not found: %s" exe-path))

  (when (and (not (called-interactively-p 'any)) exe-path)
	(setq run-exe-last-path exe-path))
  (when (and (not (called-interactively-p 'any)) args)
	(setq run-exe-last-args args))

  ;; Normalize path just for internal consistency/display
  (let* ((normalized-path (expand-file-name exe-path))
         (filename (file-name-nondirectory normalized-path))
         (process-name filename)
         (buffer-name (format "*%s-output*" filename))
         (output-buffer (get-buffer-create buffer-name))
         (process nil))

    ;; Prepare and display the output buffer
    (with-current-buffer output-buffer
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (format "--- Starting: %s %s ---\nCommand: %s\nArguments: %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        process-name
                        normalized-path
                        ;; Use the 'args' passed to the function body
                        (if args (mapconcat #'identity args " ") "<none>")))
        (goto-char (point-max))
        (setq buffer-read-only nil)))
    (display-buffer output-buffer)

    ;; Start the process
    (condition-case err
        (progn
          ;; Use the 'args' passed to the function body
		  (message "woah 1")
          (setq process (apply #'start-process
                               process-name output-buffer normalized-path args))
		  (message "woah 2")
          (set-process-sentinel process #'run-exe-async--sentinel)
		  (message "woah 3")
          (with-current-buffer output-buffer
            (setq mode-line-process `(:propertize ,process-name help-echo "Process Info")))
		  (message "woah 4")
          (message "Process '%s' started asynchronously (Args: %s)."
                   process-name (if args (prin1-to-string args) "<none>")))
      (error (message "Error starting process '%s': %s" normalized-path err)
             (with-current-buffer output-buffer
               (let ((buffer-read-only nil))
                 (goto-char (point-max))
                 (insert (format "\n--- ERROR starting process: %s ---" err))))
             (setq process nil)))
    process))

(defun my-make-file-writable ()
  "Make the current buffer's file writable (clear read-only file status).
Works on both Windows and Linux."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Buffer is not visiting a file!")
      (set-file-modes file
                      (file-modes-symbolic-to-number "u+w" (file-modes file)))
      (read-only-mode -1)
      (message "File '%s' is now writable." file))))

(defun my-projectile-related-file ()
  "Open the C++ file that corresponds to the one you are editing.

This command switches between source and header files, handling
`Impl` suffixes. For example, it can switch between `Foo.h` and
`Foo.cpp` or `FooImpl.cpp`.

* If exactly one match exists – open it automatically.
* If several matches exist – present a completing-read list.

The search is performed in the current Projectile project. If the
file is not part of a project or has an unsupported extension,
the command signals an error."
  (interactive)
  ;; 1. Grab the name/extension of the current buffer.
  (let* ((buf-file   (buffer-file-name))
         (ext        (and buf-file (file-name-extension buf-file)))
         (base       (and ext (file-name-base buf-file))))
    (unless (and base
                 (member ext '("cpp" "hpp" "h")))
      (user-error "This command only works on .cpp, .hpp, or .h files"))

    ;; 2. Compute target extensions and base names.
    (let* ((target-exts (if (string= ext "cpp")
                            '("hpp" "h")
                          '("cpp")))
           (target-bases (if (string-suffix-p "Impl" base)
                             (list base (string-remove-suffix "Impl" base))
                           (list base (concat base "Impl"))))
           (project-root   (or (projectile-acquire-root)
                               (user-error "Not in a Projectile project")))
           (all-files      (projectile-project-files project-root))
           ;; Keep files with a target base name and extension.
           (candidates     (cl-remove-if-not
                            (lambda (f)
                              (and (member (file-name-extension f) target-exts)
                                   (member (file-name-base f) target-bases)))
                            all-files)))

      (cond
       ((null candidates)
        (user-error "No corresponding file found for `%s`" base))

       ((= (length candidates) 1)
        (find-file (expand-file-name (car candidates) project-root))
        (message "Opened %s" (car candidates)))

       (t
        (let ((choice (completing-read "Select file: " candidates nil t)))
          (when choice
            (find-file (expand-file-name choice project-root))
            (message "Opened %s" choice))))))))

(defun my-projectile-configure-project ()
  "Configure the current project using CMake with Ninja for Debug build."
  (interactive)
  (let ((compilation-read-command nil) ;; disable prompt
        (command-map (if (projectile--cache-project-commands-p) projectile-compilation-cmd-map)))
    (projectile--run-project-cmd "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Debug -G \"Ninja\" -B build" command-map
                                 :show-prompt t
                                 :prompt-prefix "Compile command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-compile-use-comint-mode)))

(defun my-projectile-build-project ()
  "Compile the current project using 'cmake --build build'."
  (interactive)
  (let ((compilation-read-command nil) ;; disable prompt
        (command-map (if (projectile--cache-project-commands-p) projectile-compilation-cmd-map)))
    (projectile--run-project-cmd "cmake --build build" command-map
                                 :show-prompt nil
                                 :prompt-prefix "Compile command: "
                                 :save-buffers t
                                 :use-comint-mode projectile-compile-use-comint-mode)))

(defun smarter-begining-of-line ()
  "Move point to first non-whitespace character or beginning of line."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (eq pt (point))
	  (beginning-of-line))))


(defun mark-sexp-at-point ()
  "Like running backward-sexp, then forward sexp and mark."
  (interactive)
  (forward-sexp)
  (set-mark-command nil)
  (backward-sexp))

(defun move-line-up ()
  "Move the current line up one line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down one line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun copy-file-name ()
  "Copy the current file's filename to clipboard."
  (interactive)
  (if buffer-file-name
	  (progn
		(kill-new buffer-file-name)
		buffer-file-name)
	nill))

(defun create-cpp-include (file-path)
  "Create a c++ include from file path."
  (interactive "fFile path: ")
  (let* ((normalized-path (replace-regexp-in-string "\\\\" "/" file-path))
		 (public-pos (string-match "/Public/" normalized-path))
		 (code-pos (string-match "/Code/" normalized-path))
		 (include-path
		  (cond
		   (public-pos
			(substring normalized-path
					   (+ public-pos (length "/Public/"))))
		   (code-pos
			(substring normalized-path
					   (+ code-pos (length "/Code/"))))
		   (t normalized-path))))
	(kill-new (format "#include <%s>" include-path)))
  )

(defun create-cpp-include-from-current-buffer ()
  "Create a c++ include for the current buffer."
  (interactive)
  (create-cpp-include (copy-file-name))
  )
