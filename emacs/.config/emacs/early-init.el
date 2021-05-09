(setq gc-cons-threshold most-positive-fixnum)
(setq user-emacs-directory (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist
	     '(font . "monospace-12"))

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

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

(straight-use-package 'use-package)

(use-package general)

(use-package doom-themes
  :config (load-theme 'doom-one t))

(use-package evil
  :init (setq evil-want-keybinding nil
	      evil-undo-system 'undo-fu)
  :config (evil-mode +1))

(use-package undo-fu)
(use-package undo-fu-session
  :init (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config (global-undo-fu-session-mode))

(use-package evil-collection
  :config (evil-collection-init))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

(use-package gcmh
  :hook (emacs-startup . gcmh-mode))

(use-package magit
  :general ("C-x g" #'magit-status)
  :init (setq magit-define-global-key-bindings nil))

(use-package which-key
  :config (which-key-mode +1))

(use-package dired-x
  :straight nil
  :hook (dired-mode . dired-omit-mode))
