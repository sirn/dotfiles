;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature python
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (declare-function gemacs--eglot-format-buffer nil)
    (declare-function gemacs--eglot-organize-imports nil)
    (declare-function gemacs--python-auto-format nil)
    (declare-function gemacs--python-lsp-bin nil)
    (defvar eglot-server-programs))

  :init
  (setq python-fill-docstring-style 'django)
  (setq python-indent-guess-indent-offset-verbose nil)

  :config
  (use-feature eglot
    :demand t
    :config
    (defun gemacs--python-lsp-bin ()
      (gemacs--path-join
        (file-name-as-directory (getenv "HOME"))
        ".dotfiles/libexec/lsp/pylsp"))

    (defun gemacs--python-auto-format ()
      (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

    (add-to-list 'eglot-server-programs `(python-mode . (,(gemacs--python-lsp-bin))))
    (add-hook 'python-mode-hook #'eglot-ensure)
    (add-hook 'python-mode-hook #'gemacs--python-auto-format))

  (cond
    ((executable-find "python3") (setq python-shell-interpreter "python3"))
    ((executable-find "python2") (setq python-shell-interpreter "python2"))
    (t (setq python-shell-interpreter "python"))))
