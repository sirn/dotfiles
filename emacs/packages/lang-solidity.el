(defun custom/setup-company-solidity ()
  (set (make-local-variable 'company-backends) '(company-solidity)))

(req-package solidity-mode
  :mode ("\\.sol\\'" . solidity-mode)
  :init
  (progn
    (add-hook 'solidity-mode-hook 'custom/setup-company-solidity)
    (setq solidity-flycheck-solium-checker-active t)))
