(setq async-shell-command-buffer 'new-buffer)
(add-to-list 'display-buffer-alist
             '("^\\*Async Shell Command\\*" . display-buffer-window))

(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

(add-to-list 'default-frame-alist
             '(font . "monospace-12"))

(setq key-chord-two-keys-delay 0.02
      key-chord-one-keys-delay 0.04)

(setq find-file-visit-truename t)

(setq user-emacs-directory
      (expand-file-name "emacs/" (getenv "XDG_DATA_HOME")))

(setq inhibit-startup-screen t)

(setq use-dialog-box nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-hook-name-suffix nil)
(require 'use-package)

(use-package general)

(use-package dired-x
  :straight nil
  :hook (dired-mode-hook . dired-omit-mode)
  :init
  (setq dired-guess-shell-alist-user
        '(("\\.ogg\\'" "mpv")
          ("\\.swf\\'" "ruffle"))))

(use-package display-line-numbers
  :straight nil
  :hook (emacs-startup-hook . global-display-line-numbers-mode)
  :init
  (setq display-line-numbers-type 'relative))

(use-package evil
  :after undo-fu ;; undo-fu is needed for redo
  :hook (emacs-startup-hook . evil-mode)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-fu
  :init
  (setq evil-undo-system 'undo-fu))

(use-package time
  :straight nil
  :hook (emacs-startup-hook . display-time-mode))

(use-package battery
  :straight nil
  :hook (emacs-startup-hook . display-battery-mode))

(use-package eww
  :straight nil
  :init
  (setq eww-download-directory "~/www/"))

(use-package exwm
  :commands exwm-enable
  :init
  (add-to-list 'command-switch-alist
               '("--exwm" . (lambda (switch-string) (exwm-enable))))
  :config
  (make-process :name "sxhkd" :command '("sxhkd") :noquery t)
  (make-process :name "picom" :command '("picom") :noquery t)
  (make-process :name "pipewire" :command '("pipewire") :noquery t)
  (make-process :name "pipewire-pulse" :command '("pipewire-pulse") :noquery t)
  (make-process :name "redshift" :command '("redshift") :noquery t)

  (add-to-list 'initial-frame-alist
               '(fullscreen . fullboth))

  (add-hook 'exwm-update-class-hook
            (lambda () (exwm-workspace-rename-buffer exwm-class-name))))

(use-package gcmh
  :hook (emacs-startup-hook . gcmh-mode))

(use-package gruvbox-theme
  :hook (emacs-startup-hook . (lambda () (load-theme 'gruvbox t))))

(use-package magit
  :general
  ("C-x g" 'magit-status)
  :init
  (setq magit-define-global-key-bindings nil))

(use-package org
  :straight nil
  :hook (org-mode-hook . org-indent-mode))
