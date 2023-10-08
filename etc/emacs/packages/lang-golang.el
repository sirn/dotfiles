;; -*- lexical-binding: t; no-native-compile: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

(use-package go-mode
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))


(use-package go-ts-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--go-auto-format nil)
    (declare-function go--backward-irrelevant nil))

  :init
  (defun gemacs--go-auto-format ()
    (add-hook 'before-save-hook #'lsp-format-buffer)
    (add-hook 'before-save-hook #'lsp-organize-imports))

  (add-hook 'go-ts-mode-hook #'lsp-deferred)
  (add-hook 'go-ts-mode-hook #'gemacs--go-auto-format)
  (add-hook 'go-ts-mode-hook #'flycheck-golangci-lint-setup))


(use-package flycheck-golangci-lint)
