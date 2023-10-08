;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'apheleia-mode)
  (add-hook 'haskell-mode-hook #'flycheck-mode))
