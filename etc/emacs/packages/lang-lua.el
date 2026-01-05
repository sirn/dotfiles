;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package lua-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :hook
  ((lua-mode . apheleia-mode)
   (lua-mode . flymake-mode)))
