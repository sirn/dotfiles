;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package lua-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'lua-mode-hook #'apheleia-mode))

  (use-feature flycheck
    :demand t

    :config
    (add-hook 'lua-mode-hook #'flycheck-mode)))


(use-package pico8-mode
  :straight (:host github :repo "Kaali/pico8-mode"))
