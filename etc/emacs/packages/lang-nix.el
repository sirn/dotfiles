;; -*- lexical-binding: t -*-

(use-package nix-mode
  :init
  (use-feature lsp-mode
    :init
    (add-hook 'nix-mode-hook #'lsp)))
