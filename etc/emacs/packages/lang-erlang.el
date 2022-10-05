;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package erlang
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (add-to-list 'eglot-server-programs '(erlang-mode . ("erlang_ls")))
    (add-hook 'erlang-mode-hook #'eglot-ensure)))
