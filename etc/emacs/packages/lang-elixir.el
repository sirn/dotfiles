;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package elixir-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--elixir-auto-format nil))

  :init
  (defun gemacs--elixir-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (add-hook 'elixir-mode-hook #'eglot-ensure)
  (add-hook 'elixir-mode-hook #'flycheck-mode)
  (add-hook 'elixir-mode-hook #'gemacs--elixir-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(elixir-mode . ("elixir-language-server")))))
