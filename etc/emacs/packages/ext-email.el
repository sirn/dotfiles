(eval-when-compile
  (defvar sendmail-program))


;; No deferred load to hook notmuch with messages.
(use-package notmuch
  :straight t

  :init
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-f-is-evil 't)
  (setq message-sendmail-envelope-from 'header)
  (setq sendmail-program "msmtp")

  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "mm" 'notmuch)))


(use-package counsel-notmuch
  :after counsel
  :commands counsel-notmuch
  :straight t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "m/" 'counsel-notmuch)))
