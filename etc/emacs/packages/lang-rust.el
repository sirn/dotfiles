;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package rust-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (declare-function gemacs--rust-auto-format nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (defun gemacs--rust-auto-format ()
      (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

    (add-to-list 'eglot-server-programs '(rust-mode . ("rls")))
    (add-hook 'rust-mode-hook #'eglot-ensure)
    (add-hook 'rust-mode-hook #'gemacs--rust-auto-format)))
