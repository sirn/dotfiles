;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package typescript-mode
  :mode
  ("\\.tsx\\'"
   "\\.jsx?\\'")

  :init
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))


(use-feature typescript-ts-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--typescript-auto-format nil))

  :init
  (defun gemacs--typescript-auto-format ()
    (add-hook 'before-save-hook #'lsp-organize-imports))

  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'typescript-ts-mode-hook #'gemacs--typescript-auto-format)
  (add-hook 'typescript-ts-mode-hook #'apheleia-mode))
