;;; early-init.el -*- lexical-binding: t; -*-

;; Disable garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Load config.org
(let* ((config (expand-file-name "config" (file-name-directory load-file-name)))
       (config.org (concat config ".org"))
       (config.el  (concat config ".el")))
  (when (file-newer-than-file-p config.org config.el)
    (require 'ob-tangle)
    (org-babel-tangle-file config.org config.el "emacs-lisp"))
  (load config.el nil nil t))
