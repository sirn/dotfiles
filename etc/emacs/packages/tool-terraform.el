;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package terraform-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'terraform-mode-hook #'apheleia-mode)))
