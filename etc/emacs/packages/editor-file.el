;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dired
  :general
  ("C-x C-j" #'dired-jump)

  :config
  (pcase system-type
    ((or 'darwin 'berkeley-unix)
     (setq dired-use-ls-dired nil))))


(use-package nerd-icons-dired
  :init
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))


(use-package treemacs
  :general
  (leader
    "t t" #'treemacs)

  :config
  ;; Deferred loading
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))


(use-package osx-trash
  :when (eq system-type 'darwin)

  :preface
  (eval-when-compile
    (declare-function osx-trash-setup nil))

  :config
  (osx-trash-setup))
