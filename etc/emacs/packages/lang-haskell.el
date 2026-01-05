;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package haskell-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :hook
  ((haskell-mode . apheleia-mode)
   (haskell-mode . flymake-mode)))
