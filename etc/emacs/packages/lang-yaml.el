(use-package yaml-mode
  :mode ("\\.\\(yaml|yml\\)\\'" . yaml-mode)
  :straight t

  :init
  (add-hook 'yaml-mode-hook 'flycheck-mode))
