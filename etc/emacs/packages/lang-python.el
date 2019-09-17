(eval-when-compile
  (defvar python-shell-interpreter)
  (defvar flycheck-python-flake8-executable))


(setq python-shell-interpreter "python3")


(use-package python-mode
  :mode "\\.py\\'"

  :preface
  (eval-when-compile
    (defvar lsp-pyls-server-command)
    (defvar lsp-pyls-plugins-pycodestyle-enabled)
    (defvar lsp-pyls-plugins-mccabe-enabled)
    (defvar lsp-pyls-plugins-pyflakes-enabled)
    (declare-function lsp nil))

  :init
  (setq lsp-pyls-server-command
    (concat
      (file-name-as-directory (getenv "HOME"))
      ".dotfiles/bin/python-language-server.sh"))

  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq lsp-pyls-plugins-mccabe-enabled nil)
  (setq lsp-pyls-plugins-pyflakes-enabled nil)

  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'flycheck-mode))
