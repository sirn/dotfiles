;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package json-mode
  :init
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))


;; Bulitin; tree-sitter
(use-package json-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil)
    (declare-function flycheck-mode nil))

  :init
  (add-hook 'json-ts-mode-hook #'apheleia-mode)
  (add-hook 'json-ts-mode-hook #'eglot-ensure)
  (add-hook 'json-ts-mode-hook #'flycheck-mode)
  
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(json-ts-mode . ("vscode-json-language-server" "--stdio")))))


(use-package jq-mode)
