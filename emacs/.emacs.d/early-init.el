;;; early-init.el

;; Disable default garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Load init.org
(let* ((init (expand-file-name "init" (file-name-directory load-file-name)))
       (init.org (concat init ".org"))
       (init.el  (concat init ".el"))
       (init.elc (concat init ".elc"))
       (init.eln (concat init ".eln")))
  (when (file-newer-than-file-p init.org init.el)
    (require 'ob-tangle)
    (org-babel-tangle-file init.org init.el "emacs-lisp"))
  (if (featurep 'native-compile)
      (when (file-newer-than-file-p init.el init.eln)
        (native-compile init.el init.eln))
    (when (file-newer-than-file-p init.el init.elc)
      (byte-compile-file init.el))))
