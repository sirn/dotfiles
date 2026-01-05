;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package make-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--makefile-force-tabs nil)
    (declare-function gemacs--makefile-disable-editorconfig nil)
    (declare-function gemacs--makefile-disable-dtrt-indent nil))

  :hook
  (makefile-mode . gemacs--makefile-force-tabs)

  :config
  (with-eval-after-load 'editorconfig
    (defun gemacs--makefile-disable-editorconfig ()
      "Disable editorconfig-mode for Makefile."
      (editorconfig-mode -1))
    (add-hook 'makefile-mode-hook #'gemacs--makefile-disable-editorconfig))

  (with-eval-after-load 'dtrt-indent
    (defun gemacs--makefile-disable-dtrt-indent ()
      "Disable dtrt-indent-mode for Makefile."
      (dtrt-indent-mode -1))
    (add-hook 'makefile-mode-hook #'gemacs--makefile-disable-dtrt-indent))

  (defun gemacs--makefile-force-tabs ()
    "Force Makefile to always use tabs."
    (setq indent-tabs-mode t)))
