;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dockerfile-mode
  :init
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))


(use-package dockerfile-ts-mode)
