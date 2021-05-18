;;; early-init.el

;; Disable garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Disable extra user interface
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)

(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Default font
(add-to-list 'default-frame-alist
             '(font . "monospace-12"))

;; Change user-emacs-directory
(setq user-emacs-directory
      (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

;; Keep file system clean
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(let ((auto-save-file-name-directory
       (expand-file-name "auto-saves/" user-emacs-directory)))
  (mkdir auto-save-file-name-directory t)
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-file-name-directory t))))
(setq create-lockfiles nil)

;; Follow symlinks
(setq find-file-visit-truename t)

;; Disable saving passwords
(setq auth-source-save-behavior nil)

;; Highlight matching parenthesis
(show-paren-mode +1)

;; Whitespace
(global-whitespace-mode +1)

;; Autocomplete pairs
(electric-pair-mode +1)

;; Tab complete
(setq tab-always-indent 'complete)

;; Line wrap
(global-visual-line-mode +1)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

;; Buffer predicate
(add-to-list 'default-frame-alist
             '(buffer-predicate . buffer-file-name))

;; dired
(setq dired-listing-switches "-l -hgvAFX --group-directories-first")

;; Woman manpath
(when (executable-find "manpath")
  (with-eval-after-load "woman"
    (setq woman-manpath
          (woman-parse-colon-path
           (string-trim-right (shell-command-to-string "manpath -q") "\n")))))

;; Package manager
(setq straight-use-symlinks t)
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Theme
(straight-use-package 'doom-themes)
(load-theme 'doom-one t)

;; TTY support
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(straight-use-package 'xclip)
(add-hook 'tty-setup-hook #'xclip-mode)

;; Completion user interface
(straight-use-package 'selectrum)
(selectrum-mode +1)

(straight-use-package 'marginalia)
(marginalia-mode +1)

;; Smart garbage collection
(straight-use-package 'gcmh)
(add-hook 'emacs-startup-hook #'gcmh-mode)

;; Which key
(straight-use-package 'which-key)
(which-key-mode +1)

;; Language server protocol
(straight-use-package 'eglot)

;; Major modes
(straight-use-package 'lua-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'vimrc-mode)
(straight-use-package 'zig-mode)

;; Vim emulation
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

(straight-use-package 'evil-terminal-cursor-changer)
(add-hook 'tty-setup-hook #'evil-terminal-cursor-changer-activate)
(with-eval-after-load "evil-terminal-cursor-changer"
  (add-hook 'kill-emacs-hook
            (lambda ()
              (evil-set-cursor t)
              (etcc--evil-set-cursor))))

;; Git integration
(straight-use-package 'magit)
(setq magit-define-global-key-bindings nil)
(global-set-key (kbd "C-c g") #'magit-status)

(straight-use-package 'diff-hl)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(global-diff-hl-mode +1)
(diff-hl-flydiff-mode +1)
(diff-hl-margin-mode +1)

(with-eval-after-load "evil-terminal-cursor-changer"
  (advice-add 'diff-hl-flydiff-update :after #'etcc--evil-set-cursor))
