(defun custom/setup-tide ()
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (company-mode-on))

(req-package tide
  :require (company typescript-mode)
  :commands (tide-setup tide-mode)
  :diminish tide-mode
  :init
  (add-hook 'typescript-mode-hook 'custom/setup-tide)
  :config
  (add-hook 'before-save-hook 'tide-format-before-save))

(req-package typescript-mode
  :mode "\\.ts\\'"
  :interpreter "tss")
