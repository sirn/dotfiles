(use-package sql
  :ensure t
  :mode ("\\.sql\\'" . sql-mode))

(use-package sql-indent
  :after sql
  :ensure t)
