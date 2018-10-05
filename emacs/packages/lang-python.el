;; (eval-when-compile
;;   (declare-function lsp-python-enable nil))

;; (defun lsp-python--find-root ()
;;   "Returns the Python project root."
;;   (or (locate-dominating-file default-directory "setup.py")
;;       (locate-dominating-file default-directory "setup.cfg")
;;       (projectile-project-root)
;;       default-directory))

;; (with-eval-after-load 'lsp-mode
;;   (lsp-define-stdio-client lsp-python "python" 'lsp-python--find-root '("pyls"))
;;   (add-hook 'python-mode-hook 'lsp-python-enable))

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
    (defun setup-company-python ()
      (set (make-local-variable 'company-backends) '(company-anaconda)))
    (add-hook 'python-mode-hook 'setup-company-python)))


(use-package pipenv
  :diminish pipenv-mode
  :straight t

  :commands
  (pipenv-mode
   pipenv-activate)

  :init
  (with-eval-after-load 'projectile
    (with-eval-after-load 'flycheck
      (add-hook 'python-mode-hook 'pipenv-mode))))
