;; follow symlinks
(setq-default vc-follow-symlinks t)

;; hide buffers based on name
(defun custom--should-hide-buffer-based-on-name (name)
  (or
   (string-match "^\\*Async Shell Command\\*\\(<[0-9]+>\\)?$" name)
   (string-match "^\\*Buffer List\\*$" name)
   (string-match "^\\*Help\\*$" name)
   (string-match "^\\*Messages\\*$" name)
   (string-match "^\\*straight-process\\*$" name)
   (string-match "^magit\\(-.*\\)?: " name)))

;; configure async-shell-command
(setq-default
 async-shell-command-buffer 'new-buffer
 display-buffer-alist
 '(("^\\*Async Shell Command\\*" . display-buffer-window)))

;; configure buffer navigation to ignore "hidden" bufferse
(defun custom--should-show-buffer (buffer)
  (not (custom--should-hide-buffer-based-on-name (buffer-name buffer))))
(add-to-list
 'default-frame-alist
 '(buffer-predicate . custom--should-show-buffer))

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

;; ivy
(straight-use-package 'ivy)
(setq-default ivy-ignore-buffers '(custom--should-hide-buffer-based-on-name))
(require 'ivy)
(ivy-mode +1)

;; counsel
(straight-use-package 'counsel)
(require 'counsel)
(counsel-mode +1)

;; swiper
(straight-use-package 'swiper)
(require 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)

;; magit
(straight-use-package 'magit)
(setq-default magit-define-global-key-bindings)
(require 'magit)

;; gruvbox-theme
(straight-use-package 'gruvbox-theme)
(require 'gruvbox-theme)
(load-theme 'gruvbox t)

;; general
(straight-use-package 'general)
(setq-default
 general-override-states
 '(insert emacs hybrid normal visual motion operator replace))
(require 'general)
(general-define-key
 :states '(normal visual motion)
 :keymaps 'override

 "SPC f" 'find-file
 "SPC b" 'switch-to-buffer

 "SPC g" 'magit)

;; exwm
(straight-use-package 'exwm)

(defun custom--setup-exwm (switch-string)
  (add-to-list
   'initial-frame-alist
   '(fullscreen . fullboth))

  (start-process "sxhkd" nil "sxhkd")
  (start-process "picom" nil "picom")
  (start-process "redshift" nil "redshift")

  (set-process-query-on-exit-flag (get-process "sxhkd") nil)
  (set-process-query-on-exit-flag (get-process "picom") nil)
  (set-process-query-on-exit-flag (get-process "redshift") nil)

  ;; combine custom buffer-predicate with exwm's buffer-predicate
  (add-hook
   'exwm-init-hook
   (lambda ()
     (modify-all-frames-parameters
      '((buffer-predicate
	 . (lambda (buffer)
	     (and
	      (custom--should-show-buffer buffer)
	      (exwm-layout--other-buffer-predicate buffer))))))))
  
  (add-hook
   'exwm-exit-hook
   (lambda () (stop-process (get-process "redshift"))))

  (defun custom--exwm-update-buffer-name ()
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ": " exwm-title)))

  (add-hook 'exwm-update-class-hook 'custom--exwm-update-buffer-name)
  (add-hook 'exwm-update-title-hook 'custom--exwm-update-buffer-name)

  (require 'exwm)
  (exwm-enable))

(add-to-list
 'command-switch-alist
 '("--exwm" . custom--setup-exwm))
