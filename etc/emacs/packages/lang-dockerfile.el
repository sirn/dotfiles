(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :straight t

  :init
  (add-hook 'dockerfile-mode-hook 'flycheck-mode))
