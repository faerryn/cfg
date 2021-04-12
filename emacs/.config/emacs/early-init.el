;; set user-emacs-directory to a saner location
(setq-default
 user-emacs-directory
 (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

;; disable unused features
(setq-default
 inhibit-startup-screen t
 package-enable-at-startup nil
 use-dialog-box nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
