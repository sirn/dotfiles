;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package ruby-mode
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

;; Builtin; tree-sitter
(use-package ruby-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil))

  :hook
  ((ruby-ts-mode . apheleia-mode)
   (ruby-ts-mode . eglot-ensure)
   (ruby-ts-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(ruby-ts-mode . ("ruby-lsp"))))

  :config
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(rubyfmt . ("rubyfmt")))
    (add-to-list 'apheleia-mode-alist '(ruby-ts-mode . rubyfmt))))
