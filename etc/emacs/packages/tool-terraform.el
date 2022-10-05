;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package terraform-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :config
  (use-feature apheleia
    :demand t
    :config
    (add-hook 'terraform-mode-hook #'apheleia-mode)))
