;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package rust-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (add-to-list 'eglot-server-programs '(rust-mode . ("rls")))
    (add-hook 'rust-mode-hook #'eglot-ensure)))
