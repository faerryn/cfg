;; follow symlinks
(setq-default vc-follow-symlinks t)

;; configure async-shell-command
(setq-default
 async-shell-command-buffer 'new-buffer
 display-buffer-alist
 '(("^\\*Async Shell Command\\*" . display-buffer-window)))

;; configure backup
(setq-default
 backup-by-copying t
 backup-directory-alist
 `(("." . ,(expand-file-name "backup" user-emacs-directory))))

;; configure auto-save
(setq-default
 auto-save-file-name-transforms
 `((".*" ,(expand-file-name "auto-saves" user-emacs-directory) t)))

;; disable lockfiles
(setq-default create-lockfiles nil)

;; line numbers
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)

;; straight.el
(setq-default straight-use-package-by-default t)
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
(setq-default use-package-hook-name-suffix nil)
(require 'use-package)

;; general.el
(straight-use-package 'general)
(require 'general)

;; evil
(use-package evil
  :hook (emacs-startup-hook . evil-mode)
  :init
  (setq-default evil-want-integration t
		evil-want-keybinding nil))

;; evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; magit
(use-package magit
  :general
  ("C-x g" 'magit-status)
  :init
  (setq-default magit-define-global-key-bindings))

;; gruvbox-theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

;; exwm
(use-package exwm
  :commands exwm-enable
  :config
  ; fullscreen emacs
  (add-to-list
   'initial-frame-alist
   '(fullscreen . fullboth))

  ; set exwm buffer names
  (add-hook
   'exwm-update-class-hook
   (lambda () (exwm-workspace-rename-buffer exwm-class-name))))

; start exwm on --exwm
(add-to-list
 'command-switch-alist
 '("--exwm"
   . (lambda (switch-string)
       ; startup daemons
       (make-process :name "sxhkd" :command '("sxhkd") :noquery t)
       (make-process :name "picom" :command '("picom") :noquery t)
       (make-process :name "redshift" :command '("redshift") :noquery t)
       (exwm-enable))))
