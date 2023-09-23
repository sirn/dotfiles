;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package nix-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function gemacs--nix-auto-format nil))

  :config
  (use-feature lsp-mode
    :demand t

    :config
    (defun gemacs--nix-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer))

    (add-hook 'nix-mode-hook #'lsp-deferred)
    (add-hook 'nix-mode-hook #'gemacs--nix-auto-format)))
