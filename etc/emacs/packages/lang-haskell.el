;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package haskell-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :config
  (use-feature apheleia
    :demand t
    :config
    (add-hook 'haskell-mode-hook #'apheleia-mode)))
