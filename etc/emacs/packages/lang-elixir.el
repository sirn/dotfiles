;; -*- lexical-binding: t -*-

(use-package elixir-mode)


(use-package alchemist
  :after elixir-mode
  :diminish alchemist-mode

  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'elixir-mode-hook #'flycheck-mode))


(use-package flycheck-mix
  :after elixir-mode
  :commands flycheck-mix-setup

  :init
  (add-hook 'elixir-mode-hook #'flycheck-mix-setup))
