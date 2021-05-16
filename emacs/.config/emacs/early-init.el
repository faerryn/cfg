;;; early-init.el

;; Disabling GC
(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Disable extra UI
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)

(menu-bar-mode -1)
(tooltip-mode -1)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Set user-emacs-directory to a more appropriate location
(setq user-emacs-directory
      (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Show paren mode
(show-paren-mode +1)

;; Electric pair mode
(electric-pair-mode +1)

;; Tab complete
(setq tab-always-indent 'complete)

;; Line wrap
(global-visual-line-mode +1)

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
(setq straight-use-symlinks t)
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

;; vertico
(straight-use-package 'vertico)
(vertico-mode +1)

;; corfu
(straight-use-package 'corfu)
(corfu-global-mode +1)

;; marginalia
(straight-use-package 'marginalia)
(marginalia-mode +1)

;; Smarter GC
(straight-use-package 'gcmh)
(add-hook 'emacs-startup-hook #'gcmh-mode)

;; which-key
(straight-use-package 'which-key)
(which-key-mode +1)

;; TTY support
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(straight-use-package 'xclip)
(add-hook 'tty-setup-hook #'xclip-mode)

(straight-use-package 'evil-terminal-cursor-changer)
(add-hook 'tty-setup-hook #'evil-terminal-cursor-changer-activate)
(add-hook 'kill-emacs-hook (lambda () (evil-set-cursor t)))

;; eglot
(straight-use-package 'eglot)

;; rust
(straight-use-package 'rust-mode)

;; zig
(straight-use-package 'zig-mode)

;; doom-one theme
(straight-use-package 'doom-themes)
(load-theme 'doom-one t)

;; evil mode
(straight-use-package 'evil)
(setq evil-want-Y-yank-to-eol t
      evil-want-keybinding nil
      evil-undo-system 'undo-fu)
(evil-mode +1)

(straight-use-package 'undo-fu)
(straight-use-package 'undo-fu-session)
(global-undo-fu-session-mode)

(straight-use-package 'evil-collection)
(evil-collection-init)

;; Magit
(straight-use-package 'magit)
(setq magit-define-global-key-bindings nil)
(global-set-key (kbd "C-c g") #'magit-status)
