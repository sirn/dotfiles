;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dired
  :config
  (pcase system-type
    ((or 'darwin 'berkeley-unix)
     (setq dired-use-ls-dired nil))))


(use-package dired-sidebar
  :general
  (leader
    "TT" #'dired-sidebar-toggle-sidebar
    "Tt" #'dired-sidebar-toggle-with-current-directory)

  :config
  (setq dired-sidebar-theme 'ascii))


(use-package osx-trash
  :when (eq system-type 'darwin)

  :preface
  (eval-when-compile
    (declare-function osx-trash-setup nil))

  :config
  (osx-trash-setup))
