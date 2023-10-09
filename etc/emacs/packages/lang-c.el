;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package cc-mode
  :init
  (add-to-list 'major-mode-remap-alist '(cc-mode . c-ts-mode)))


(use-package c-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (add-hook 'c-ts-mode-hook #'apheleia-mode)
  (add-hook 'c-ts-mode-hook #'flycheck-mode))
