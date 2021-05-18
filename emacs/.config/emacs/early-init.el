;;; early-init.el

(let* ((config (expand-file-name "config" (file-name-directory load-file-name)))
       (config.org (concat config ".org"))
       (config.el (concat config ".el"))
       (config.elc (concat config ".elc")))
  (when (file-newer-than-file-p config.org config.el)
    (require 'ob-tangle)
    (org-babel-tangle-file config.org config.el "emacs-lisp"))
  (when (file-newer-than-file-p config.el config.elc)
    (byte-compile-file config.el))
  (load config.elc nil nil t))
