(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :straight t

  :preface
  (eval-when-compile
    (declare-function lsp nil))

  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'lsp)
  (add-hook 'rust-mode-hook 'flycheck-mode))
