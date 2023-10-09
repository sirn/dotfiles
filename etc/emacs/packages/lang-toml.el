;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package toml-mode
  :init
  (add-to-list 'major-mode-remap-alist '(toml-mode . toml-ts-mode)))


;; Builtin; tree-sitter
(use-package toml-ts-mode)
