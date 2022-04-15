;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; ControlMaster is configured in ~/.ssh/config; make TRAMP reuse
  ;; that persistent session instead of making its own.
  (setq tramp-use-ssh-controlmaster-options nil))
