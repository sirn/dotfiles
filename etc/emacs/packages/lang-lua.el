;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package lua-mode
  :init
  (add-hook 'lua-mode-hook #'apheleia-mode)
  (add-hook 'lua-mode-hook #'flycheck-mode))
