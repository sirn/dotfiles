;; -*- lexical-binding: t -*-

(use-package nim-mode
  :init
  (use-feature lsp
    :init
    (add-hook 'nim-mode-hook #'lsp)))
