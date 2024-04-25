;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package ruby-mode
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

;; Builtin; tree-sitter
(use-package ruby-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :init
  (defun gemacs--ruby-flycheck ()
    (setq-local flycheck-checker 'ruby-standard)
    (setq-local flycheck-disabled-checkers '(ruby-rubocop)))

  (add-hook 'ruby-ts-mode-hook #'flycheck-mode)
  (add-hook 'ruby-ts-mode-hook #'apheleia-mode)
  (add-hook 'ruby-ts-mode-hook #'gemacs--ruby-flycheck)

  :config
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(rubyfmt . ("rubyfmt")))
    (add-to-list 'apheleia-mode-alist '(ruby-ts-mode . rubyfmt))))
