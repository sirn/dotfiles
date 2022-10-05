;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package lua-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :config
  (use-feature apheleia
    :demand t
    :config
    (add-hook 'lua-mode-hook #'apheleia-mode)))


(use-package pico8-mode
  :straight (:host github :repo "Kaali/pico8-mode"))
