;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package css-mode
  :init
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))


;; Bulitin; tree-sitter
(use-package css-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil)
    (declare-function flymake-mode nil))

  :hook
  ((css-ts-mode . apheleia-mode)
   (css-ts-mode . eglot-ensure)
   (css-ts-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(css-ts-mode . ("vscode-css-language-server" "--stdio")))))
