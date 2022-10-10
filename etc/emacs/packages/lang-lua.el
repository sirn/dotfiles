;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package lua-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function flymake-lua-load nil)
    (declare-function flymake-mode nil))

  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'lua-mode-hook #'apheleia-mode))

  (use-feature flymake
    :demand t

    :config
    (add-hook 'lua-mode-hook #'flymake-mode))

  (use-feature flymake-lua
    :demand t

    :config
    (add-hook 'lua-mode-hook #'flymake-lua-load)))


(use-package pico8-mode
  :straight (:host github :repo "Kaali/pico8-mode"))


(use-package flymake-lua)
