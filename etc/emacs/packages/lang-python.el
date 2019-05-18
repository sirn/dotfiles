(eval-when-compile
  (defvar python-shell-interpreter))

(setq python-shell-interpreter "python3")

(use-package anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :diminish anaconda-mode
  :straight t

  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (with-eval-after-load 'eldoc
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))


(use-package blacken
  :commands blacken-mode
  :diminish blacken-mode
  :straight t

  :init
  (add-hook 'python-mode-hook 'blacken-mode))


(use-package company-anaconda
  :after anaconda-mode
  :commands company-anaconda
  :straight t

  :init
  (with-eval-after-load 'company
    (defun gr/setup-company-python ()
      (set (make-local-variable 'company-backends) '(company-anaconda)))
    (add-hook 'python-mode-hook 'gr/setup-company-python)))
