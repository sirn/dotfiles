;; -*- lexical-binding: t -*-

(use-package elixir-mode
  :preface
  (eval-when-compile
    (defvar lsp-clients-elixir-server-executable))

  :init
  (setq lsp-clients-elixir-server-executable "elixir-ls"))
