;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package cc-mode
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))


(use-package c-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil))

  :hook
  ((c-ts-mode . apheleia-mode)
   (c-ts-mode . eglot-ensure)
   (c-ts-mode . flymake-mode)
   (c++-ts-mode . apheleia-mode)
   (c++-ts-mode . eglot-ensure)
   (c++-ts-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((c-ts-mode c++-ts-mode) . ("clangd")))))
