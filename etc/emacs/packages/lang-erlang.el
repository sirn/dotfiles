;; -*- lexical-binding: t -*-

(use-package erlang
  :init
  (use-feature lsp-mode
    :init
    (add-hook 'erlang-mode-hook #'lsp)))
