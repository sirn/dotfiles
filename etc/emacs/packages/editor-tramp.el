;; -*- lexical-binding: t -*-

(use-feature tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-use-ssh-controlmaster-options nil))
