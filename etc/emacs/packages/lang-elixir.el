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

  :init
  (defun gemacs--elixir-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (add-hook 'elixir-ts-mode-hook #'eglot-ensure)
  (add-hook 'elixir-ts-mode-hook #'flymake-mode)
  (add-hook 'elixir-ts-mode-hook #'gemacs--elixir-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(elixir-ts-mode . ("elixir-ls")))))
