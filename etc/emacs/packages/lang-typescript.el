;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package typescript-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (declare-function gemacs--typescript-auto-format nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t

    :config
    (defun gemacs--typescript-auto-format ()
      (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

    (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server")))
    (add-hook 'typescript-mode-hook #'eglot-ensure)
    (add-hook 'typescript-mode-hook #'gemacs--typescript-auto-format)))
