;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package sh-script
  :mode
  (("\\.bats\\'" . bash-ts-mode))

  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil))

  :hook
  ((bash-ts-mode . apheleia-mode)
   (bash-ts-mode . eglot-ensure)
   (bash-ts-mode . flymake-mode))

  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server" "start")))))
