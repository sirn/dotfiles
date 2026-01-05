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

  :init
  (add-hook 'css-ts-mode-hook #'apheleia-mode)
  (add-hook 'css-ts-mode-hook #'eglot-ensure)
  (add-hook 'css-ts-mode-hook #'flymake-mode)
  
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(css-ts-mode . ("vscode-css-language-server" "--stdio")))))
