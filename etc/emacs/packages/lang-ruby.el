;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package ruby-mode
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

;; Builtin; tree-sitter
(use-package ruby-ts-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--ruby-auto-format nil))

  :init
  (defun gemacs--ruby-auto-format ()
    (add-hook 'before-save-hook #'lsp-format-buffer)
    (add-hook 'before-save-hook #'lsp-organize-imports))

  (add-hook 'ruby-ts-mode-hook #'lsp-deferred)
  (add-hook 'ruby-ts-mode-hook #'gemacs--ruby-auto-format))
