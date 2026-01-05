;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package sh-script
  :mode
  (("\\.bats\\'" . bash-ts-mode))

  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil))

  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (add-hook 'bash-ts-mode-hook #'apheleia-mode)
  (add-hook 'bash-ts-mode-hook #'eglot-ensure)
  (add-hook 'bash-ts-mode-hook #'flymake-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server" "start")))))
