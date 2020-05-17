;; -*- lexical-binding: t -*-

(use-package nim-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--nimscript-mode-maybe nil))

  :config
  (advice-add 'nimscript-mode-maybe :around #'gemacs--nimscript-mode-maybe)

  (defun gemacs--nimscript-mode-maybe (_func &rest _args)
    "Fix NimScript detection. The original `nimscript-mode-maybe'
were causing `nim-mode' to always error with `Wrong type argument'.

This function is meant to workaround such issue by force enabling
`nimscript-mode' all the time when visiting nims or nimble file."
    (nimscript-mode))

  (use-feature lsp
    :config
    (add-hook 'nim-mode-hook #'lsp)))
