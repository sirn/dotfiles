(use-package js2-mode
  :mode "\\.js\\'"
  :straight t)


(use-package js2-refactor
  :after js2-mode
  :commands js2-refactor-mode
  :diminish js2-refactor-mode
  :straight t

  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode))


(use-package json-mode
  :mode "\\.json\\'"
  :straight t)


(use-package skewer-mode
  :after js2-mode
  :commands skewer-mode
  :diminish skewer-mode
  :straight t

  :init
  (add-hook 'js2-mode-hook 'skewer-mode))
