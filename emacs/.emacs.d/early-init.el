;;; early-init.el -*- lexical-binding: t; -*-

;; Disable garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Load init.org
(let* ((init.org (expand-file-name "init.org" user-emacs-directory))
       (init.el (expand-file-name "init.el" user-emacs-directory)))
  (when (file-newer-than-file-p init.org init.el)
    (require 'ob-tangle)
    (org-babel-tangle-file init.org init.el "emacs-lisp")))
