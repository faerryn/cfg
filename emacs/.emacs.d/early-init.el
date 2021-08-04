;;; early-init.el -*- lexical-binding: t; -*-

;; Disable garbage collection
(custom-set-variables '(gc-cons-threshold most-positive-fixnum))

;; Disable package.el
(custom-set-variables '(package-enable-at-startup nil))

;; Load config.org
(let* ((config.org (expand-file-name "config.org" user-emacs-directory))
       (config.el (expand-file-name "config.el" user-emacs-directory)))
  (when (file-newer-than-file-p config.org config.el)
    (require 'ob-tangle)
    (org-babel-tangle-file config.org config.el "emacs-lisp"))
  (load config.el nil nil t))
