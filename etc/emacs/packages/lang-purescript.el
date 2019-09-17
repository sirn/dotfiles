(use-package purescript-mode
  :commands (purescript-mode turn-on-purescript-indentation)
  :mode "\\.purs\\'"
  :straight t

  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  (add-hook 'purescript-mode-hook 'flycheck-mode)
  (with-eval-after-load 'purescript-indentation
    '(diminish 'purescript-indentation-mode)))


(use-package psc-ide
  :after purescript-mode
  :commands psc-ide-mode
  :diminish psc-ide-mode
  :straight t

  :init
  (add-hook 'purescript-mode-hook 'psc-ide-mode))
