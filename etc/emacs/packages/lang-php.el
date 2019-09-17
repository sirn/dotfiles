(use-package php-mode
  :interpreter "php"
  :mode ("\\.php\\'" . php-mode)
  :straight t

  :init
  (add-hook 'php-mode-hook 'flycheck-mode))
