;; -*- lexical-binding: t -*-

(use-package jsonnet-mode
  :config
  (use-feature apheleia
    :config
    (add-to-list 'apheleia-formatters '(jsonnetfmt . ("jsonnetfmt"
                                                       "--indent" "2"
                                                       "--max-blank-lines" "2"
                                                       "--sort-imports"
                                                       "--string-style" "s"
                                                       "--comment-style" "s"
                                                       "--" "-")))
    (add-to-list 'apheleia-mode-alist '(jsonnet-mode . jsonnetfmt))))
