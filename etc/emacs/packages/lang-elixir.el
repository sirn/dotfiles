;; -*- lexical-binding: t -*-

(use-package elixir-mode
  :preface
  (eval-when-compile
    (defvar lsp-clients-elixir-server-executable))

  :init
  (use-feature lsp-mode
    :init
    (setq lsp-elixir-server-command '("elixir-ls"))
    (add-hook 'elixir-mode-hook #'lsp)))
