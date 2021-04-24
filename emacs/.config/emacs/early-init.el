;; disable garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; disable package.el
(setq package-enable-at-startup nil)

;; settings that change the frame's size
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Platform specific fonts
(when (string= system-type "gnu/linux")
  (add-to-list 'default-frame-alist '(font . "monospace-12")))
(when (string= system-type "windows-nt")
  (add-to-list 'default-frame-alist '(font . "Consolas-12")))

;; tangle init.org
(let ((init.org (expand-file-name "init.org" user-emacs-directory))
      (init.el (expand-file-name "init.el" user-emacs-directory)))
  (when (file-newer-than-file-p init.org init.el)
    (require 'ob-tangle)
    (org-babel-tangle-file init.org init.el "^emacs-lisp$")))
