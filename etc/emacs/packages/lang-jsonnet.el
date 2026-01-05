;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package jsonnet-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :hook
  (jsonnet-mode . apheleia-mode)

  :config
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters
     '(jsonnetfmt . ("jsonnetfmt"
                      "--indent" "2"
                      "--max-blank-lines" "2"
                      "--sort-imports"
                      "--string-style" "s"
                      "--comment-style" "s"
                      "--" "-")))
    (add-to-list 'apheleia-mode-alist '(jsonnet-mode . jsonnetfmt))))
