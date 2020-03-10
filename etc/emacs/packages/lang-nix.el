(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode)
  :straight t

  :init
  (add-hook 'nix-mode-hook 'flycheck-mode))
