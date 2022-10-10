;; -*- lexical-binding: t; no-native-compile: t -*-

(gemacs-when-compiletime (executable-find "notmuch")
  ;; Using distro notmuch here since notmuch-emacs requires the version
  ;; between the emacs client and notmuch to match (due to changes in
  ;; exchange format)
  (use-feature notmuch
    ;; Not exposed via autoload by Notmuch
    :commands notmuch

    :general
    (leader
      "mm" #'notmuch)

    :custom
    (message-send-mail-function #'message-send-mail-with-sendmail)
    (message-sendmail-f-is-evil 't)
    (message-sendmail-envelope-from 'header)
    (sendmail-program "msmtp")))
