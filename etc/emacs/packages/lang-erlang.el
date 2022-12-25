;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package erlang
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--erlang-auto-format nil))

  :config
  (use-feature lsp-mode
    :demand t

    :config
    (defun gemacs--erlang-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports))

    (add-hook 'erlang-mode-hook #'lsp)
    (add-hook 'elixir-mode-hook #'gemacs--erlang-auto-format)))
