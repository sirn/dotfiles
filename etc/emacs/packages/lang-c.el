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

  :init
  (add-hook 'c-ts-mode-hook #'apheleia-mode)
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook #'flymake-mode)
  (add-hook 'c++-ts-mode-hook #'apheleia-mode)
  (add-hook 'c++-ts-mode-hook #'eglot-ensure)
  (add-hook 'c++-ts-mode-hook #'flymake-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((c-ts-mode c++-ts-mode) . ("clangd")))))
