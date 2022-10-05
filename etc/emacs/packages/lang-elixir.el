;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package elixir-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (add-to-list 'eglot-server-programs '(elixir-mode . ("elixir-ls")))
    (add-hook 'elixir-mode-hook #'eglot-ensure)))
