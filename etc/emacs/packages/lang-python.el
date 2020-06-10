;; -*- lexical-binding: t -*-

(use-feature python
  :preface
  (eval-when-compile
    (defvar lsp-pyls-server-command))

  :init
  (setq python-fill-docstring-style 'django)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq lsp-pyls-server-command
    (gemacs--path-join
      (file-name-as-directory (getenv "HOME"))
      ".dotfiles/bin/pyls"))

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

  (use-feature lsp-pyls
    :config
    (setq lsp-pyls-plugins-pycodestyle-enabled nil)
    (setq lsp-pyls-plugins-mccabe-enabled nil)
    (setq lsp-pyls-plugins-pyflakes-enabled nil)
    (setq lsp-pyls-plugins-flake8-enabled t)))
