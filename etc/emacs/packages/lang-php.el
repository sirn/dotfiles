;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package php-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (add-hook 'php-mode-hook #'apheleia-mode)
  (add-hook 'php-mode-hook #'flycheck-mode))
