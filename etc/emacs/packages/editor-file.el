;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dired
  :general
  ("C-x C-j" #'dired-jump)

  :config
  (pcase system-type
    ((or 'darwin 'berkeley-unix)
     (setq dired-use-ls-dired nil))))


(use-package dired-sidebar
  :preface
  (eval-when-compile
    (declare-function gemacs--dired-sidebar-setup nil))

  :general
  (leader
    "t T" #'dired-sidebar-toggle-sidebar
    "t t" #'dired-sidebar-toggle-with-current-directory)

  :config
  (setq dired-sidebar-theme 'ascii))


(use-package osx-trash
  :when (eq system-type 'darwin)

  :preface
  (eval-when-compile
    (declare-function osx-trash-setup nil))

  :config
  (osx-trash-setup))
