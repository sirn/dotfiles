;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package terraform-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (add-hook 'terraform-mode-hook #'apheleia-mode))
