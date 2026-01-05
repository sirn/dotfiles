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

  :hook
  ((conf-mode . tempel-setup-capf)
   (prog-mode . tempel-setup-capf)
   (text-mode . tempel-setup-capf))

  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions))))
