(req-package company-go
  :require company
  :commands company-go
  :init
  (add-hook 'go-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends)
		   '(company-go)))))

(req-package flycheck-gometalinter
  :require flycheck
  :commands flycheck-gometalinter-setup
  :init
  (add-hook 'go-mode-hook #'flycheck-gometalinter-setup))

(req-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(req-package go-eldoc
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))
