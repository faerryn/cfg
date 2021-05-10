;; Speed up startup by disabling GC
(setq gc-cons-threshold most-positive-fixnum)

;; Disable extra UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Set user-emacs-directory to a more appropriate location
(setq user-emacs-directory (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

;; Set default font
(add-to-list 'default-frame-alist
	     '(font . "monospace-12"))

;; Set backup
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; dired
(autoload 'dired-omit-mode "dired-x")
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

;; general.el
(straight-use-package 'general)
(require 'general)

;; doom-one theme
(use-package doom-themes
  :config (load-theme 'doom-one t))

;; evil mode
(use-package evil
  :init (setq evil-want-keybinding nil
	      evil-undo-system 'undo-fu)
  :config (evil-mode +1))

(use-package undo-fu)
(use-package undo-fu-session
  :config (global-undo-fu-session-mode))

(use-package evil-collection)

;; Smarter GC
(use-package gcmh
  :defer 1
  :config (gcmh-mode +1))

;; Magit
(use-package magit
  :general ("C-x g" #'magit-status)
  :init (setq magit-define-global-key-bindings nil))

;; which-key
(use-package which-key
  :config (which-key-mode +1))
