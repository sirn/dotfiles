(defun setup-tide ()
  (tide-setup)
  (tide-hl-identifier-mode t)

  (with-eval-after-load 'flycheck
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (flycheck-mode t))

  (with-eval-after-load 'eldoc
    (eldoc-mode t))

  (with-eval-after-load 'company
    (company-mode-on)))


(use-package tide
  :after typescript-mode
  :diminish tide-mode
  :straight t

  :commands
  (tide-setup
   tide-mode)

  :init
  (add-hook 'typescript-mode-hook 'setup-tide)

  :config
  (add-hook 'before-save-hook 'tide-format-before-save))


(use-package typescript-mode
  :interpreter "tss"
  :mode "\\.ts\\'"
  :straight t)
