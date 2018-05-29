(req-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save t))

(req-package racer
  :require (company eldoc rust-mode)
  :diminish racer-mode
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))
