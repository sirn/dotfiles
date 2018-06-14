(setq python-shell-interpreter "python3")


(use-package anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :diminish anaconda-mode
  :ensure t

  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (with-eval-after-load 'eldoc
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))


(use-package blacken
  :commands blacken-mode
  :diminish blacken-mode
  :ensure t

  :init
  (add-hook 'python-mode-hook 'blacken-mode))


(use-package company-anaconda
  :after anaconda-mode
  :commands company-anaconda
  :ensure t

  :init
  (with-eval-after-load 'company
    (defun setup-company-python ()
      (set (make-local-variable 'company-backends) '(company-anaconda)))
    (add-hook 'python-mode-hook 'setup-company-python)))


(use-package pipenv
  :diminish pipenv-mode
  :ensure t

  :commands
  (pipenv-mode
   pipenv-activate)

  :init
  (with-eval-after-load 'projectile
    (with-eval-after-load 'flycheck
      (add-hook 'python-mode-hook 'pipenv-mode))))