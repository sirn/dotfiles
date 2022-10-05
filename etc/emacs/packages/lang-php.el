;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package php-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :config
  (use-feature apheleia
    :demand t
    :config
    (add-hook 'php-mode-hook #'apheleia-mode)))
