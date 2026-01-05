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

  :hook
  ((go-ts-mode . eglot-ensure)
   (go-ts-mode . flymake-mode)
   (go-ts-mode . gemacs--go-auto-format))

  :init
  (defun gemacs--go-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))))
