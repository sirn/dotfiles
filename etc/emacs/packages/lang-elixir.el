;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package elixir-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--eglot-format-buffer nil)
    (declare-function gemacs--eglot-organize-imports nil)
    (declare-function gemacs--elixir-auto-format nil))

  :config
  (use-feature eglot
    :demand t

    :config
    (defun gemacs--elixir-auto-format ()
      (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

    (add-to-list 'eglot-server-programs '(elixir-mode . ("elixir-language-server")))
    (add-hook 'elixir-mode-hook #'eglot-ensure)
    (add-hook 'elixir-mode-hook #'gemacs--elixir-auto-format)))
