;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature cc-mode
  :init
  (add-to-list 'major-mode-remap-alist '(cc-mode . c-ts-mode)))


(use-feature c-ts-mode
  :init
  (add-hook 'c-ts-mode-hook #'apheleia-mode)
  (add-hook 'c-ts-mode-hook #'flycheck-mode))
