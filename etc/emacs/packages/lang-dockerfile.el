;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dockerfile-mode
  :init
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))


;; Builtins; tree-sitter
(use-package dockerfile-ts-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil))

  :hook
  ((dockerfile-ts-mode . eglot-ensure)
   (dockerfile-ts-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))))
