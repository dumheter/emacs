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

;; Font
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (add-to-list 'default-frame-alist '(font . "Cascadia Mono PL-9"))
    (set-face-attribute 'default t
		        :font "Cascadia Mono PL-9"
			))))

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (set-face-attribute 'default nil :height 100))))

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; custom stuff in custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file)
  )

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
  (setq scroll-conservatively 101) ;; Only scroll one step once cursor leaves window.
  (setq frame-title-format
		'("Emacs @ " (:eval (if (buffer-file-name)
							 (abbreviate-file-name (buffer-file-name))
							 (buffer-name)))))
  (defun smarter-begining-of-line ()
    "Move point to first non-whitespace character or beginning of line."
    (interactive)
    (let ((pt (point)))
      (back-to-indentation)
      (when (eq pt (point))
		(beginning-of-line))))
  (hl-line-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (delete-selection-mode 1) ;; Deletion commands work on regions.

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
					  
  )


(use-package cc-mode
  ;; Don't indent after namespace.
  :hook (c++-mode . (lambda ()
					  (c-set-offset 'innamespace 0)
					  (c-set-offset 'namespace-open 0)))
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

;;(use-package gptel
;;  :config
;;  (setq gptel-model 'o3-mini
;;		gptel-backend
;;		(gptel-make-openai "Github UwU"
;;		  :host "models.inference.ai.azure.com"
;;		  ;;:endpoint "/chat/completions?api-version=2024-05-01-preview"
;;		  :stream t
;;		  :key "YOUR KEY HERE"
;;		  :models '(o3-mini)
;;		  )
;;		)
;;  )

(use-package copilot-chat
  :ensure t
  )

(use-package org
  :hook (org-mode . visual-line-mode)
  )

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t)
  )
