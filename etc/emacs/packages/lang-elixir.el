;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package elixir-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--elixir-auto-format nil)
    (defvar lsp-elixir-server-command))

  :config
  (use-feature lsp-mode
    :demand t

    :init
    (setq lsp-elixir-server-command '("elixir-ls"))

    :config
    (defun gemacs--elixir-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports))

    (add-hook 'elixir-mode-hook #'lsp-deferred)
    (add-hook 'elixir-mode-hook #'gemacs--elixir-auto-format)))
