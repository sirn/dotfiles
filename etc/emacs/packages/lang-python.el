;; -*- lexical-binding: t -*-

(use-feature python
  :preface
  (eval-when-compile
    (declare-function lsp-register-custom-settings nil)
    (defvar lsp-pylsp-server-command))

  :init
  (setq python-fill-docstring-style 'django)
  (setq python-indent-guess-indent-offset-verbose nil)

  (use-feature lsp-mode
    :init
    (add-hook 'python-mode-hook #'lsp))

  :config
  (defun gemacs--python-fix-outline-mode-config ()
    "Prevent `python-mode' from overriding `outline-minor-mode' config.
If this hook is not used, then `python-mode' will override even a
file-local setting of e.g. `outline-regexp' with its own setting."
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp))

  (defun gemacs--python-no-reindent-on-colon ()
    "Don't reindent on typing a colon.
See https://emacs.stackexchange.com/a/3338/12534."
    (setq electric-indent-chars (delq ?: electric-indent-chars)))

  (cond
    ((executable-find "python3") (setq python-shell-interpreter "python3"))
    ((executable-find "python2") (setq python-shell-interpreter "python2"))
    (t (setq python-shell-interpreter "python")))

  (use-feature lsp-pylsp
    :config
    (setq lsp-pylsp-plugins-pycodestyle-enabled nil)
    (setq lsp-pylsp-plugins-mccabe-enabled nil)
    (setq lsp-pylsp-plugins-pyflakes-enabled nil)
    (setq lsp-pylsp-plugins-flake8-enabled t)
    (lsp-register-custom-settings
      '(("pylsp.plugins.mypy-ls.enabled" t t)
        ("pylsp.plugins.mypy-ls.live_mode" nil t)
        ("pylsp.plugins.pyls_isort.enabled" t t)))))
