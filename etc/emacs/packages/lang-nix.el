;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package nix-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
    (add-hook 'nix-mode-hook #'eglot-ensure)))
