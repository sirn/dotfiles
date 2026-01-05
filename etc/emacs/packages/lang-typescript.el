;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package typescript-mode
  :mode
  ("\\.tsx\\'"
   "\\.jsx?\\'")

  :init
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))


;; Builtin; tree-sitter
(use-package typescript-ts-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--typescript-auto-format nil))

  :init
  (defun gemacs--typescript-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'flymake-mode)
  (add-hook 'typescript-ts-mode-hook #'gemacs--typescript-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))))
