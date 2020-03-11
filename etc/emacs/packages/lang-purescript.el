;; -*- lexical-binding: t -*-

(use-package purescript-mode
  :commands (purescript-mode turn-on-purescript-indentation)

  :init
  (add-hook 'purescript-mode-hook #'turn-on-purescript-indentation))


(use-package psc-ide
  :after purescript-mode
  :commands psc-ide-mode

  :init
  (add-hook 'purescript-mode-hook #'psc-ide-mode))
