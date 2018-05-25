(defun custom/setup-company-go ()
  (set (make-local-variable 'company-backends) '(company-go)))

(req-package company-go
  :require (company go-mode)
  :commands company-go
  :init
  (add-hook 'go-mode-hook 'custom/setup-company-go))

(req-package flycheck-gometalinter
  :require (flycheck go-mode)
  :commands flycheck-gometalinter-setup
  :init
  (add-hook 'go-mode-hook #'flycheck-gometalinter-setup))

(req-package go-eldoc
  :require go-mode
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(req-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))
