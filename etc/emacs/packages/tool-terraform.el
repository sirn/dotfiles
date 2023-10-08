;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package terraform-mode
  :init
  (add-hook 'terraform-mode-hook #'apheleia-mode))
