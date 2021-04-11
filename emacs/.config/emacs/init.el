;; ignore buffers with names of form *<string>* for the purposes of navigation
(set-frame-parameter
 nil 'buffer-predicate
 (lambda (buffer)
   (or
    (string-match "^\\*scratch\\*$" (buffer-name buffer))
    (not (string-match "^\\*" (buffer-name buffer))))))

;; configure backup
(setq-default
 backup-by-copying t
 backup-directory-alist
 `(("." . ,(expand-file-name "backup" user-emacs-directory))))

;; configure auto-save
(setq-default
 auto-save-file-name-transform
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
(require 'magit)

;; gruvbox-theme
(straight-use-package 'gruvbox-theme)
(require 'gruvbox-theme)
(load-theme 'gruvbox t)
