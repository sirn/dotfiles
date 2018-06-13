(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")


(use-package js2-refactor
  :after js2-mode
  :commands js2-refactor-mode
  :diminish js2-refactor-mode
  :ensure t

  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode))


(use-package json-mode
  :ensure t
  :mode "\\.json\\'")


(use-package skewer-mode
  :after js2-mode
  :commands skewer-mode
  :diminish skewer-mode
  :ensure t

  :init
  (add-hook 'js2-mode-hook 'skewer-mode))
