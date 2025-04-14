;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package python
  :custom
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil)

  :preface
  (eval-when-compile
    (declare-function gemacs--python-auto-format nil)
    (defvar flycheck-python-pycompile-executable))

  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (dolist (name '("python" "python2" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

  (cond
    ((executable-find "python3") (setq python-shell-interpreter "python3"))
    ((executable-find "python2") (setq python-shell-interpreter "python2"))
    (t (setq python-shell-interpreter "python")))

  (defun gemacs--python-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (defun gemacs--python-disable-flycheck ()
    (flycheck-mode -1))

  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'gemacs--python-disable-flycheck)
  (add-hook 'python-ts-mode-hook #'gemacs--python-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
    (setq-default eglot-workspace-configuration
      '((pylsp
          (plugins
            (black (enabled . t))
            (flake8 (enabled . t))
            (pycodestyle (enabled . nil))
            (pyflakes (enabled . t))))))))
