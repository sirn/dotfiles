;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package erlang
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (declare-function gemacs--eglot-format-buffer nil)
    (declare-function gemacs--eglot-organize-imports nil)
    (declare-function gemacs--erlang-auto-format nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (defun gemacs--erlang-auto-format ()
      (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

    (add-to-list 'eglot-server-programs '(erlang-mode . ("erlang_ls")))
    (add-hook 'erlang-mode-hook #'eglot-ensure)
    (add-hook 'elixir-mode-hook #'gemacs--erlang-auto-format)))
