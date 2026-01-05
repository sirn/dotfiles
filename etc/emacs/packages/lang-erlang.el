;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package erlang
  :preface
  (eval-when-compile
    (declare-function gemacs--erlang-auto-format nil))

  :init
  (defun gemacs--erlang-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (add-hook 'erlang-mode-hook #'eglot-ensure)
  (add-hook 'erlang-mode-hook #'flymake-mode)
  (add-hook 'erlang-mode-hook #'gemacs--erlang-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(erlang-mode . ("erlang_ls")))))
