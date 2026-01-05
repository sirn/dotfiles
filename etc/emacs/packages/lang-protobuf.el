;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package protobuf-mode
  :mode "\\.proto\\'"

  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil))

  :hook
  ((protobuf-mode . flymake-mode)
   (protobuf-mode . apheleia-mode)
   (protobuf-mode . eglot-ensure))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(protobuf-mode . ,(eglot-alternatives
                                     '(("protols")
                                       ("buf" "lsp"))))))
                 
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters
                 '(buf . ("buf" "format" filepath)))
    (add-to-list 'apheleia-mode-alist
                 '(protobuf-mode . buf))))