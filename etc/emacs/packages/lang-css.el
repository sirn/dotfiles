;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package css-mode
  :init
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))


;; Bulitin; tree-sitter
(use-package css-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (add-hook 'css-ts-mode-hook #'apheleia-mode)
  (add-hook 'css-ts-mode-hook #'flycheck-mode))
