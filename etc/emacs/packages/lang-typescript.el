(use-package typescript-mode
  :interpreter "tss"
  :mode "\\.ts\\'"
  :straight t

  :preface
  (eval-when-compile
    (declare-function lsp nil))

  :init
  (add-hook 'typescript-mode-hook 'lsp)
  (add-hook 'typescript-mode-hook 'flycheck-mode))
