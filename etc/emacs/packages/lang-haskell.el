;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package haskell-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (add-hook 'haskell-mode-hook #'apheleia-mode)
  (add-hook 'haskell-mode-hook #'flymake-mode))
