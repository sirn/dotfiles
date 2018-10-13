(use-package auth-source
  :straight t

  :preface
  (eval-when-compile
    (defvar auth-sources))

  :config
  (setq auth-sources '("~/.authinfo.gpg")))


(use-package pinentry
  :straight t)


(use-package epa
  :after pinentry
  :straight t

  :preface
  (eval-when-compile
    (defvar epa-pinentry-mode))

  :init
  (setq epa-pinentry-mode 'loopback))


(use-package epg
  :straight t
  :after (epa keychain-environment)

  :preface
  (declare-function pinentry-start nil)

  :config
  (pinentry-start))
