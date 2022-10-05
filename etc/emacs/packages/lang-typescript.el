;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package typescript-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server")))
    (add-hook 'typescript-mode-hook #'eglot-ensure)))
