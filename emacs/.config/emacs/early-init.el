(setq-default
 package-enable-at-startup nil
 user-emacs-directory (expand-file-name "emacs" (or (getenv "XDG_DATA_HOME") (getenv "APPDATA")))
 inhibit-startup-screen t
 use-dialog-box nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
