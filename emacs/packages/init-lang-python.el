(defun custom/setup-company-python ()
  (set (make-local-variable 'company-backends) '(company-anaconda)))

(setq python-shell-interpreter "python3")

(req-package anaconda-mode
  :require eldoc
  :commands anaconda-mode
  :diminish anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(req-package company-anaconda
  :require (company anaconda-mode)
  :commands company-anaconda
  :init
  (add-hook 'python-mode-hook 'custom/setup-company-python))

(req-package pipenv
  :require (flycheck projectile)
  :commands (pipenv-mode pipenv-activate)
  :diminish pipenv-mode
  :init
  (add-hook 'python-mode-hook 'pipenv-mode))

(req-package blacken
  :commands (blacken-mode)
  :diminish blacken-mode
  :init
  (add-hook 'python-mode-hook 'blacken-mode))
