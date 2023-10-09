;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package lua-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (add-hook 'lua-mode-hook #'apheleia-mode)
  (add-hook 'lua-mode-hook #'flycheck-mode))
