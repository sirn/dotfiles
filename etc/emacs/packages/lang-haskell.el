;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package haskell-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function flymake-haskell-multi-load nil)
    (declare-function flymake-mode nil))

  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'haskell-mode-hook #'apheleia-mode))

  (use-feature flymake
    :demand t

    :config
    (add-hook 'sh-mode-hook #'flymake-mode))

  (use-feature flymake-haskell-multi
    :demand t

    :config
    (add-hook 'sh-mode-took #'flymake-haskell-multi-load)))


(use-package flymake-haskell-multi)
