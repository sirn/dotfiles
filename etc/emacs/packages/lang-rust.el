;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package rust-mode
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))


;; Builtin; tree-sitter
(use-package rust-ts-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--rust-auto-format nil))

  :init
  (defun gemacs--rust-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (add-hook 'rust-ts-mode-hook #'eglot-ensure)
  (add-hook 'rust-ts-mode-hook #'flymake-mode)
  (add-hook 'rust-ts-mode-hook #'gemacs--rust-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))))
