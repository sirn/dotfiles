;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package elixir-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--elixir-auto-format nil)
    (defvar lsp-elixir-server-command))

  :init
  (setq lsp-elixir-server-command '("elixir-ls"))

  (defun gemacs--elixir-auto-format ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'elixir-mode-hook #'lsp-deferred)
  (add-hook 'elixir-mode-hook #'gemacs--elixir-auto-format))
