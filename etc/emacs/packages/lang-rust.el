;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package rust-mode
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))


;; Builtin; tree-sitter
(use-package rust-ts-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--rust-auto-format nil))

  :hook
  ((rust-ts-mode . eglot-ensure)
   (rust-ts-mode . flymake-mode)
   (rust-ts-mode . gemacs--rust-auto-format))

  :init
  (defun gemacs--rust-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))))
