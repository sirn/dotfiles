;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package terraform-mode
  :init
  (defun gemacs--terraform-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (add-hook 'terraform-mode-hook #'eglot-ensure)
  (add-hook 'terraform-mode-hook #'flymake-mode)
  (add-hook 'terraform-mode-hook #'gemacs--terraform-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))))
