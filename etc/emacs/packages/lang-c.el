;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature cc-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :config
  (use-feature apheleia
    :demand t
    :config
    (add-hook 'cc-mode-hook #'apheleia-mode)))


(use-package flycheck-pkg-config)
