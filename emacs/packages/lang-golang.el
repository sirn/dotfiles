(use-package company-go
  :after (company go-mode)
  :commands company-go
  :ensure t

  :init
  (defun setup-company-go ()
    (set (make-local-variable 'company-backends) '(company-go)))
  (add-hook 'go-mode-hook 'setup-company-go))


(use-package flycheck-gometalinter
  :after (flycheck go-mode)
  :commands flycheck-gometalinter-setup
  :ensure t

  :init
  (add-hook 'go-mode-hook 'flycheck-gometalinter-setup))


(use-package go-eldoc
  :after go-mode
  :commands go-eldoc-setup
  :ensure t

  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package go-mode
  :ensure t
  :interpreter "go"
  :mode "\\.go\\'"

  :init
  (add-hook 'before-save-hook 'gofmt-before-save))
