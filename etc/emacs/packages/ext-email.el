;; -*- lexical-binding: t; no-native-compile: t -*-

;; Nix is managing notmuch due to dependency on notmuch binary.
;;
;; Also we have to use the distro notmuch here since notmuch-emacs
;; requires the version between the emacs client and notmuch to match
;; due to exchange format
(gemacs-when-compiletime (locate-library "notmuch")
  (use-package notmuch
    ;; Not exposed via autoload by Notmuch
    :commands notmuch

    :general
    (leader
      "A m m" #'notmuch
      "A m c" #'notmuch-mua-new-mail)

    :custom
    (message-send-mail-function #'message-send-mail-with-sendmail)
    (message-sendmail-f-is-evil 't)
    (message-sendmail-envelope-from 'header)
    (notmuch-search-oldest-first nil)
    (sendmail-program "msmtp")))
