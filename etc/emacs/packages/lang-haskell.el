;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package haskell-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'haskell-mode-hook #'apheleia-mode))

  (use-feature flycheck
    :demand t

    :config
    (add-hook 'haskell-mode-hook #'flycheck-mode)))
