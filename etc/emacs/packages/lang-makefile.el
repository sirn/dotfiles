;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature make-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--makefile-force-tabs nil)
    (declare-function gemacs--makefile-disable-editorconfig nil)
    (declare-function gemacs--makefile-disable-dtrt-indent nil))

  :config
  (use-feature editorconfig
    :config
    (defun gemacs--makefile-disable-editorconfig ()
      "Disable editorconfig-mode for Makefile."
      (editorconfig-mode -1))

    (add-hook 'makefile-mode-hook #'gemacs--makefile-disable-editorconfig))

  (use-feature dtrt-indent
    :config
    (defun gemacs--makefile-disable-dtrt-indent ()
      "Disable dtrt-indent-mode for Makefile."
      (dtrt-indent-mode -1))
    (add-hook 'makefile-mode-hook #'gemacs--makefile-disable-dtrt-indent))

  (defun gemacs--makefile-force-tabs ()
    "Force Makefile to always use tabs."
    (setq indent-tabs-mode t))
  (add-hook 'makefile-mode-hook #'gemacs--makefile-force-tabs))
