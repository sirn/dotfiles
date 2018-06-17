(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :straight t)

(use-package sql-indent
  :after sql
  :straight t)
