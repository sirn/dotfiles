;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package terraform-mode
  :hook
  ((terraform-mode . eglot-ensure)
   (terraform-mode . flymake-mode)
   (terraform-mode . gemacs--terraform-auto-format))

  :init
  (defun gemacs--terraform-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))))
