;; disable unused features
(setq-default
 inhibit-startup-screen t
 package-enable-at-startup nil
 use-dialog-box nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; tangle init.org
(let
    ((init.org (expand-file-name "init.org" user-emacs-directory))
     (init.el (expand-file-name "init.el" user-emacs-directory)))
  (unless (file-newer-than-file-p init.el init.org)
    (require 'ob-tangle)
    (org-babel-tangle-file init.org init.el "emacs-lisp")))

;; set user-emacs-directory to a saner location
(setq-default
 user-emacs-directory
 (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))
