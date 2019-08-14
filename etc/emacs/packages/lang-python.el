(eval-when-compile
  (defvar python-shell-interpreter))


(setq python-shell-interpreter "python3")

;; Taken from:
;; https://github.com/lunaryorn/.emacs.d/blob/master/lisp/flycheck-virtualenv.el)

(declare-function python-shell-calculate-exec-path "python")

(defun gr/flycheck-venv-executable-find (executable)
  "Find an EXECUTABLE in the current virtualenv if any."
  (if (bound-and-true-p python-shell-virtualenv-root)
      (let ((exec-path (python-shell-calculate-exec-path)))
        (executable-find executable))
    (executable-find executable)))

(defun gr/flycheck-venv-setup ()
  "Setup Flycheck for the current virtualenv."
  (setq-local flycheck-executable-find 'gr/flycheck-venv-executable-find))


(use-package anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :diminish anaconda-mode
  :straight t

  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'gr/flycheck-venv-setup)
  (with-eval-after-load 'eldoc
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))


(use-package company-anaconda
  :after anaconda-mode
  :commands company-anaconda
  :straight t

  :init
  (with-eval-after-load 'company
    (defun gr/setup-company-python ()
      (set (make-local-variable 'company-backends) '(company-anaconda)))
    (add-hook 'python-mode-hook 'gr/setup-company-python)))
