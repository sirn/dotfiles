;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package rust-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--rust-auto-format nil))

  :config
  (use-feature lsp-mode
    :demand t

    :config
    (defun gemacs--rust-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports))

    (add-hook 'rust-mode-hook #'lsp-deferred)
    (add-hook 'rust-mode-hook #'gemacs--rust-auto-format)))
