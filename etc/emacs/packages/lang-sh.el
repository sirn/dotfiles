;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature sh-script
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (add-hook 'sh-base-mode-hook #'apheleia-mode)
  (add-hook 'sh-base-mode-hook #'flycheck-mode))
