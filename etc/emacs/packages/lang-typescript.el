;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package typescript-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--typescript-auto-format nil))

  :config
  (use-feature lsp-mode
    :demand t

    :config
    (defun gemacs--typescript-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports))

    (add-hook 'typescript-mode-hook #'lsp)
    (add-hook 'typescript-mode-hook #'gemacs--typescript-auto-format)))
