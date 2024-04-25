;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package rust-mode
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))

;; Builtin; tree-sitter
(use-package rust-ts-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--rust-auto-format nil))

  :init
  (defun gemacs--rust-auto-format ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'rust-ts-mode-hook #'lsp-deferred)
  (add-hook 'rust-ts-mode-hook #'gemacs--rust-auto-format))
