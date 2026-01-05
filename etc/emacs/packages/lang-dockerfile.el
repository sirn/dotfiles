;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dockerfile-mode
  :init
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))


;; Builtins; tree-sitter
(use-package dockerfile-ts-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil))

  :init
  (add-hook 'dockerfile-ts-mode-hook #'eglot-ensure)
  (add-hook 'dockerfile-ts-mode-hook #'flymake-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))))
