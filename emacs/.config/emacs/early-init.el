;;; early-init.el

;; Disable package.el
(setq package-enable-at-startup nil)

;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Disable extra user interface
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)

(menu-bar-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)

;; Change user-emacs-directory
(setq user-emacs-directory
      (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

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
         (concat
          "https://raw.githubusercontent.com"
          "/raxod502/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Dark theme
(straight-use-package 'doom-themes)
(load-theme 'doom-one t)
