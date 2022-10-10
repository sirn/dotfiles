;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package nix-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--nix-auto-format nil))

  :config
  (use-feature eglot
    :demand t

    :config
    (defun gemacs--nix-auto-format ()
      (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

    (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
    (add-hook 'nix-mode-hook #'eglot-ensure)
    (add-hook 'nix-mode-hook #'gemacs--nix-auto-format)))
