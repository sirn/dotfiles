;; -*- lexical-binding: t -*-

(use-package nim-mode
  :config
  (use-feature lsp
    :config
    (add-hook 'nim-mode-hook #'lsp)))
