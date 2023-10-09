;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package tramp
  :custom
  ;; ControlMaster is configured in ~/.ssh/config; make TRAMP reuse
  ;; that persistent session instead of making its own.
  (tramp-use-ssh-controlmaster-options nil)

  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
