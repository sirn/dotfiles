;; -*- lexical-binding: t -*-

(use-package rust-mode
  :init
  (use-feature lsp-mode
    :init
    (add-hook 'rust-mode-hook #'lsp)))
