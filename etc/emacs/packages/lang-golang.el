(use-package company-go
  :after go-mode
  :commands company-go
  :straight t

  :init
  (with-eval-after-load 'company
    (defun gr/setup-company-go ()
      (set (make-local-variable 'company-backends) '(company-go)))
    (add-hook 'go-mode-hook 'gr/setup-company-go)))


(use-package flycheck-gometalinter
  :after go-mode
  :commands flycheck-gometalinter-setup
  :straight t

  :init
  (add-hook 'go-mode-hook 'flycheck-gometalinter-setup))


(use-package go-eldoc
  :after go-mode
  :commands go-eldoc-setup
  :straight t

  :init
  (with-eval-after-load 'eldoc
    (add-hook 'go-mode-hook 'go-eldoc-setup)))


(use-package go-mode
  :straight t
  :interpreter "go"
  :mode "\\.go\\'"

  :init
  (add-hook 'go-mode-hook 'flycheck-mode))
