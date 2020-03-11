;; -*- lexical-binding: t -*-

(use-package notmuch
  :defer 1
  :commands notmuch

  :preface
  (eval-when-compile
    (defvar sendmail-program))

  :init
  (setq message-send-mail-function #'message-send-mail-with-sendmail)
  (setq message-sendmail-f-is-evil 't)
  (setq message-sendmail-envelope-from 'header)
  (setq sendmail-program "msmtp")

  :leader
  ("mm" #'notmuch))


(use-package counsel-notmuch
  :after counsel
  :commands counsel-notmuch

  :leader
  ("m/" #'counsel-notmuch))
