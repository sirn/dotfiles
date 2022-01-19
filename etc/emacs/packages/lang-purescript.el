;; -*- lexical-binding: t -*-

(use-package purescript-mode
  :init
  (add-hook 'purescript-mode-hook #'turn-on-purescript-indentation))


(use-package psc-ide
  :init
  (add-hook 'purescript-mode-hook #'psc-ide-mode))
