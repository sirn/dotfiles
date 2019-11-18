(use-package alchemist
  :after elixir-mode
  :diminish alchemist-mode
  :straight t

  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (add-hook 'elixir-mode-hook 'flycheck-mode))


(use-package elixir-mode
  :straight t
  :interpreter "elixir"
  :mode ("\\.exs?\\'" "\\.elixir\\'"))


(use-package flycheck-mix
  :after elixir-mode
  :commands flycheck-mix-setup
  :straight t

  :init
  (add-hook 'elixir-mode-hook 'flycheck-mix-setup))
