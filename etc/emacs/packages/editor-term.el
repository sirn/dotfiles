;; -*- lexical-binding: t; no-native-compile: t -*-

(defun gemacs--term-setup ()
  (setq-local evil-insert-state-cursor 'box)
  (evil-insert-state))


;; Builtin
(use-package term
  :init
  (with-eval-after-load 'with-editor
    (add-hook 'term-mode-hook #'with-editor-export-editor))

  :config
  (add-hook 'term-mode-hook #'gemacs--term-setup))


;; Builtin
(use-package eshell
  :general
  (leader
    "'e" #'eshell)

  :init
  (with-eval-after-load 'with-editor
    (add-hook 'eshell-mode-hook #'with-editor-export-editor))

  :config
  (add-hook 'eshell-mode-hook #'gemacs--term-setup))


;; Builtin
(use-package vterm
  :general
  (leader
    "'v" #'vterm)

  :custom
  (vterm-shell "fish")

  :init
  (with-eval-after-load 'with-editor
    (add-hook 'vterm-mode-hook #'with-editor-export-editor))

  :config
  (add-hook 'vterm-mode-hook #'gemacs--term-setup))


(use-package multi-vterm
  :preface
  (eval-when-compile
    (declare-function multi-vterm-project nil))

  :general
  (leader
    "''" #'multi-vterm)

  :init
  (with-eval-after-load 'project
    (general-with-eval-after-load 'general
      (general-define-key :keymaps 'project-prefix-map "'" #'multi-vterm-project)
      (add-to-list 'project-switch-commands '(multi-vterm-project "VTerm") t)))

  (with-eval-after-load 'with-editor
    (add-hook 'multi-vterm-mode-hook #'with-editor-export-editor)))
