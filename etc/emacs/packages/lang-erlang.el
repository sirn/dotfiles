;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package erlang
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--erlang-auto-format nil))

  :init
  (defun gemacs--erlang-auto-format ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'erlang-mode-hook #'lsp-deferred)
  (add-hook 'elixir-mode-hook #'gemacs--erlang-auto-format))
