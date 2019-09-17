(use-package groovy-mode
  :mode ("\\.groovy\\'" . groovy-mode)
  :straight t

  :init
  (add-hook 'groovy-mode-hook 'flycheck-mode))
