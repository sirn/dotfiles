(use-package haskell-mode
  :mode "\\.hs\\'"
  :straight t

  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode))
