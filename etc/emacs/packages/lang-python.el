;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package python
  :custom
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil)

  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--python-auto-format nil)
    (defvar flycheck-python-pycompile-executable))

  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (dolist (name '("python" "python2" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

  (defun gemacs--python-auto-format ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (cond
    ((executable-find "python3") (setq python-shell-interpreter "python3"))
    ((executable-find "python2") (setq python-shell-interpreter "python2"))
    (t (setq python-shell-interpreter "python")))

  (add-hook 'python-ts-mode-hook #'lsp-deferred)
  (add-hook 'python-ts-mode-hook #'gemacs--python-auto-format))
