(use-package solidity-mode
  :ensure t
  :mode ("\\.sol\\'" . solidity-mode)

  :init
  (setq solidity-flycheck-solium-checker-active t)
  (defun setup-company-solidity ()
    (set (make-local-variable 'company-backends) '(company-solidity)))
  (add-hook 'solidity-mode-hook 'setup-company-solidity))
