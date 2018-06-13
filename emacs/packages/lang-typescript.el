(defun setup-tide ()
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (company-mode-on))


(use-package tide
  :ensure t
  :after typescript-mode
  :diminish tide-mode

  :commands
  (tide-setup
   tide-mode)

  :init
  (add-hook 'typescript-mode-hook 'setup-tide)

  :config
  (add-hook 'before-save-hook 'tide-format-before-save))


(use-package typescript-mode
  :ensure t
  :interpreter "tss"
  :mode "\\.ts\\'")
