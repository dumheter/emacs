;; # Help
;;
;; ## Not loading this file
;; If windows doesnt use .emacs.d default folder, look for what init
;; file was used by calling
;;   > `describe-variable` user-init-file
;; Then load this file from there with
;;   > (load "~/.emacs.d/init.el)
;;
;; ## lsp-bridge doesnt work.
;; Search for `App Execution Aliases` and turn off python.

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(setq-default intent-tabs-mode nil)
(setq make-backup-files nil)

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

(use-package emacs
  :ensure nil ;; buildin package
  :bind (("C-a" . smarter-begining-of-line))
  :config
  (defun smarter-begining-of-line ()
    "Move point to first non-whitespace character or beginning of line."
    (interactive)
    (let ((pt (point)))
      (back-to-indentation)
      (when (eq pt (point))
	(beginning-of-line))))
  )

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  :bind (("C-s" . 'swiper-isearch)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c f" . counsel-recentf)
	 ("C-x r" . counsel-rg)
	 ("M-y" . counsel-yank-pop)
	 )
  :config
  (setq counsel-yank-pop-separator "\n--------------------------------\n")
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

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("C:/dev/" "D:/dev/" "G:/dev/" "~/dev/"))
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recently-active)
  )

(use-package counsel-projectile
  :after projectile
  :ensure t
  :config
  (counsel-projectile-mode 1)
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

(add-to-list 'load-path (expand-file-name "~/lsp-bridge"))

(use-package lsp-bridge
  :load-path "~/lsp-bridge"
  :config
  (setq lsp-bridge-enable-signature-help t)
  (global-lsp-bridge-mode)
  )
