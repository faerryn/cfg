;; Speed up startup by disabling GC
(setq gc-cons-threshold most-positive-fixnum)

;; Disable extra UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set user-emacs-directory to a more appropriate location
(setq user-emacs-directory
      (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

;; follow symlinks
(setq vc-follow-symlinks t)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

;; Set default font
(add-to-list 'default-frame-alist
	     '(font . "monospace-12"))

;; Set backup
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; Buffer predicate
(add-to-list 'default-frame-alist
	     '(buffer-predicate . buffer-file-name))

;; dired
(autoload 'dired-omit-mode "dired-x")
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; doom-one theme
(straight-use-package 'doom-themes)
(load-theme 'doom-one t)

;; evil mode
(straight-use-package 'evil)
(setq evil-want-keybinding nil
      evil-undo-system 'undo-fu)
(require 'evil)
(evil-mode +1)

(straight-use-package 'undo-fu)
(require 'undo-fu)
(straight-use-package 'undo-fu-session)
(require 'undo-fu-session)
(global-undo-fu-session-mode)

(straight-use-package 'evil-collection)
(require 'evil-collection)
(evil-collection-init)

;; Magit
(straight-use-package 'magit)
(setq magit-define-global-key-bindings nil)
(autoload 'magit-status "magit")
(global-set-key (kbd "C-c g") #'magit-status)

;; which-key
(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode +1)

;; Smarter GC
(straight-use-package 'gcmh)
(require 'gcmh)
(add-hook 'emacs-startup-hook #'gcmh-mode)
