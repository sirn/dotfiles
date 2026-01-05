;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package go-mode
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))


;; Builtin; tree-sitter
(use-package go-ts-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--go-auto-format nil)
    (declare-function go--backward-irrelevant nil))

  :init
  (defun gemacs--go-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook #'flymake-mode)
  (add-hook 'go-ts-mode-hook #'gemacs--go-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))))
