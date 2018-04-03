(defun custom/setup-company-python ()
  (set (make-local-variable 'company-backends) '(company-anaconda)))

(setq python-shell-interpreter "python3")
(defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)
(defvaralias 'flycheck-python-pycompile-executable 'python-shell-interpreter)
(defvaralias 'flycheck-python-pylint-executable 'python-shell-interpreter)

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
