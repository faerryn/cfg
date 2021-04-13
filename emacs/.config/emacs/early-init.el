;; disable garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; disable package.el
(setq package-enable-at-startup nil)

;; clean FS
(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil)

;; disbale splash-screen
(setq inhibit-startup-screen t)

;; clean GUI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq use-dialog-box nil)

;; tangle init.org
(let
    ((init.org (expand-file-name "init.org" user-emacs-directory))
     (init.el (expand-file-name "init.el" user-emacs-directory)))
  (when (file-newer-than-file-p init.org init.el)
    (require 'ob-tangle)
    (org-babel-tangle-file init.org init.el "emacs-lisp")))

;; sane user-emacs-directory
(setq
 user-emacs-directory
 (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))
