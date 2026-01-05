;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package erlang
  :preface
  (eval-when-compile
    (declare-function gemacs--erlang-auto-format nil))

  :hook
  ((erlang-mode . eglot-ensure)
   (erlang-mode . flymake-mode)
   (erlang-mode . gemacs--erlang-auto-format))

  :init
  (defun gemacs--erlang-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(erlang-mode . ("erlang_ls")))))
