(use-package purescript-mode
  :commands (purescript-mode turn-on-purescript-indentation)
  :ensure t
  :mode "\\.purs\\'"

  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  (with-eval-after-load 'purescript-indentation
    '(diminish 'purescript-indentation-mode)))


(use-package psc-ide
  :after purescript-mode
  :commands psc-ide-mode
  :diminish psc-ide-mode
  :ensure t

  :init
  (add-hook 'purescript-mode-hook 'psc-ide-mode))
