;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature cc-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function flymake-mode nil))

  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'cc-mode-hook #'apheleia-mode))

  (use-feature flymake
    :demand t

    :config
    (add-hook 'cc-mode-hook #'flymake-mode)))
