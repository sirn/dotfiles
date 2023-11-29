;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package sh-script
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (add-hook 'sh-base-mode-hook #'apheleia-mode)
  (add-hook 'sh-base-mode-hook #'flycheck-mode))
