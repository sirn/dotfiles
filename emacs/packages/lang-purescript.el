(req-package purescript-mode
  :mode "\\.purs\\'"
  :commands (purescript-mode turn-on-purescript-indentation)
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  :config
  (eval-after-load "purescript-indentation" '(diminish 'purescript-indentation-mode)))

(req-package psc-ide
  :require (company flycheck)
  :diminish psc-ide-mode
  :commands psc-ide-mode
  :init
  (add-hook 'purescript-mode-hook 'psc-ide-mode))
