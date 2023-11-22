;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package tramp
  :custom
  ;; ControlMaster is configured in ~/.ssh/config; make TRAMP reuse
  ;; that persistent session instead of making its own.
  (tramp-use-ssh-controlmaster-options nil)

  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; https://adam.kruszewski.name/2023-05-07-opondoas-emacs-tramp-and-gentoo.html
  (add-to-list 'tramp-methods
    `("doas"
       (tramp-login-program         "doas")
       (tramp-login-args            (("-u" "%u") ("su - %u")))
       (tramp-remote-shell          ,tramp-default-remote-shell)
       (tramp-remote-shell-args     ("-c"))
       (tramp-connection-timeout    10)
       (tramp-session-timeout       300)
       (tramp-password-previous-hop t))))
