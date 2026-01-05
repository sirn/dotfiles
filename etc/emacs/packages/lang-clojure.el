;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package clojure-mode
  :init
  (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))


(use-package clojure-ts-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil))

  :hook
  ((clojure-ts-mode . eglot-ensure)
   (clojure-ts-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(clojure-ts-mode . ("clojure-lsp")))))
