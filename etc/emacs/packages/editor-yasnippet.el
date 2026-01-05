;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package tempel
  :general
  ("M-+" #'tempel-complete)
  ("M-*" #'tempel-insert)

  (leader
   "e y s" #'tempel-insert
   "e y c" #'tempel-complete)

  :custom
  (tempel-path
   (expand-file-name "~/.dotfiles/etc/emacs/templates/*.eld"))

  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))
