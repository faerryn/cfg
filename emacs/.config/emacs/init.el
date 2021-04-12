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

;; install straight.el
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

;; evil
(straight-use-package 'evil)
(setq-default evil-want-integration t
	      evil-want-keybinding nil)
(require 'evil)
(evil-mode +1)

;; evil-collection
(straight-use-package 'evil-collection)
(require 'evil-collection)
(evil-collection-init)

;; magit
(straight-use-package 'magit)
(setq-default magit-define-global-key-bindings)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; gruvbox-theme
(straight-use-package 'gruvbox-theme)
(require 'gruvbox-theme)
(load-theme 'gruvbox t)

;; exwm
(straight-use-package 'exwm)

(defun custom--setup-exwm (switch-string)
  ;; fullscreen emacs
  (add-to-list
   'initial-frame-alist
   '(fullscreen . fullboth))

  ;; set exwm buffer names
  (add-hook
   'exwm-update-class-hook
   (lambda () (exwm-workspace-rename-buffer (concat exwm-class-name))))

  ;; startup daemons
  (make-process :name "sxhkd" :command '("sxhkd") :noquery t)
  (make-process :name "picom" :command '("picom") :noquery t)
  (make-process :name "redshift" :command '("redshift") :noquery t)

  ;; start exwm
  (require 'exwm)
  (exwm-enable))

;; start exwm on --exwm
(add-to-list
 'command-switch-alist
 '("--exwm" . custom--setup-exwm))
