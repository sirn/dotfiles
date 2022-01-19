;; -*- lexical-binding: t -*-

(when (executable-find "notmuch")
  ;; Using distro notmuch here since notmuch-emacs requires the version
  ;; between the emacs client and notmuch to match (due to changes in
  ;; exchange format)
  (use-feature notmuch
    :preface
    (eval-when-compile
      (defvar sendmail-program))

    :leader
    ("mm" #'notmuch)

    :init
    (setq message-send-mail-function #'message-send-mail-with-sendmail)
    (setq message-sendmail-f-is-evil 't)
    (setq message-sendmail-envelope-from 'header)
    (setq sendmail-program "msmtp")))
