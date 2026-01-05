;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package elixir-mode
  :init
  (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode)))


;; Builtin; tree-sitter
(use-package elixir-ts-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--elixir-auto-format nil)
    (declare-function eglot-ensure nil))

  :hook
  ((elixir-ts-mode . eglot-ensure)
   (elixir-ts-mode . flymake-mode)
   (elixir-ts-mode . gemacs--elixir-auto-format))

  :init
  (defun gemacs--elixir-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(elixir-ts-mode . ("elixir-ls")))))
