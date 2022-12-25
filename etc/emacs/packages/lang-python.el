;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature python
  :custom
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil)

  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--python-auto-format nil)
    (defvar lsp-pylsp-server-command)
    (defvar flycheck-python-pycompile-executable))

  :config
  (use-feature flycheck
    :config
    (dolist (name '("python" "python2" "python3"))
      (add-to-list 'safe-local-variable-values
                   `(flycheck-python-pycompile-executable . ,name))))

  (use-feature lsp-mode
    :demand t

    :init
    (setq lsp-pylsp-server-command
      (gemacs--path-join
        (file-name-as-directory (getenv "HOME"))
        ".dotfiles/libexec/lsp/pylsp"))

    :config
    (defun gemacs--python-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports))

    (add-hook 'python-mode-hook #'lsp)
    (add-hook 'python-mode-hook #'gemacs--python-auto-format))

  (cond
    ((executable-find "python3") (setq python-shell-interpreter "python3"))
    ((executable-find "python2") (setq python-shell-interpreter "python2"))
    (t (setq python-shell-interpreter "python"))))
