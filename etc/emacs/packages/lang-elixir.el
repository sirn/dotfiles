;; -*- lexical-binding: t -*-

(use-package elixir-mode
  :config
  (use-feature lsp-clients
    :config
    (setq lsp-clients-elixir-server-executable "elixir-language-server")))
