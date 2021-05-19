;;; early-init.el

;; Disable default garbage collection
(setq gc-cons-threshold most-positive-fixnum)

;; Load config.org
(let* ((config (expand-file-name "config" (file-name-directory load-file-name)))
       (config.org (concat config ".org"))
       (config.el  (concat config ".el"))
       (config.elc (concat config ".elc"))
       (config.eln (concat config ".eln")))
  (when (file-newer-than-file-p config.org config.el)
    (require 'ob-tangle)
    (org-babel-tangle-file config.org config.el "emacs-lisp"))
  (if (featurep 'native-compile)
      (progn
        (when (file-newer-than-file-p config.el config.eln)
          (native-compile config.el config.eln))
        (load config.eln nil nil t))
    (when (file-newer-than-file-p config.el config.elc)
      (byte-compile-file config.el))
    (load config.elc nil nil t)))
