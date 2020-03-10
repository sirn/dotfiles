(use-package js2-mode
  :mode "\\.js\\'"
  :straight t

  :preface
  (eval-when-compile
    (declare-function lsp nil))

  :init
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'js2-mode-hook 'flycheck-mode))


(use-package json-mode
  :mode "\\.json\\'"
  :straight t)
