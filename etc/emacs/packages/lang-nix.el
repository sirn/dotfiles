;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package nix-mode
  :init
  (use-feature lsp-mode
    :init
    (add-hook 'nix-mode-hook #'lsp)))
