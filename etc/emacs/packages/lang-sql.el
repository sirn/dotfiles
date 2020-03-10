(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :straight t

  :init
  (add-hook 'sql-mode-hook 'flycheck-mode))


(use-package sql-indent
  :after sql
  :straight t)
