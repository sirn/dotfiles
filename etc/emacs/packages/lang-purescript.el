;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package purescript-mode
  :preface
  (eval-when-compile
    (declare-function turn-on-purescript-indentation nil))

  :init
  (add-hook 'purescript-mode-hook #'turn-on-purescript-indentation))


(use-package psc-ide
  :preface
  (eval-when-compile
    (declare-function psc-ide-mode nil))

  :init
  (add-hook 'purescript-mode-hook #'psc-ide-mode))
