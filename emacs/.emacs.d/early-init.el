;;; early-init.el -*- lexical-binding: t; -*-

;; Disable default garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Disable default package manager
(setq package-enable-at-startup nil)

;; Load init.org
(let* ((init (expand-file-name "init" (file-name-directory load-file-name)))
       (init.org (concat init ".org"))
       (init.el  (concat init ".el")))
  (when (file-newer-than-file-p init.org init.el)
    (require 'ob-tangle)
    (org-babel-tangle-file init.org init.el "emacs-lisp")))
