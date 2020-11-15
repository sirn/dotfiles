;; -*- lexical-binding: t -*-

(use-package typescript-mode
  :init
  (use-feature lsp-mode
    :init
    (add-hook 'typescript-mode-hook #'lsp)))
