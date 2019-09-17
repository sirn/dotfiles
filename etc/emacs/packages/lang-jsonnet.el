(use-package jsonnet-mode
  :straight t

  :mode
   ("\\.jsonnet\\'" . jsonnet-mode)
   ("\\.libsonnet\\'" . jsonnet-mode)

  :init
  (add-hook 'jsonnet-mode-hook 'flycheck-mode))
