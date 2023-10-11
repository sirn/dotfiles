;; -*- lexical-binding: t; no-native-compile: t -*-

(defun gemacs--term-setup ()
  (setq-local evil-insert-state-cursor 'box)
  (evil-insert-state))


;; Builtin
(use-package term
  :config
  (add-hook 'term-mode-hook #'gemacs--term-setup))


;; Builtin
(use-package eshell
  :general
  (leader
    "'e" #'eshell)

  :config
  (add-hook 'eshell-mode-hook #'gemacs--term-setup))


;; Builtin
(use-package vterm
  :general
  (leader
    "'v" #'vterm)

  :custom
  (vterm-shell "zsh")

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
      (add-to-list 'project-switch-commands '(multi-vterm-project "VTerm") t))))
