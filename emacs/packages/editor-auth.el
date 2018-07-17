(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package pinentry
  :straight t)

(use-package epa
  :after pinentry
  :straight t

  :init
  (setq epa-pinentry-mode 'loopback))

(use-package epg
  :straight t
  :after (epa keychain-environment)

  :preface
  (declare-function pinentry-start nil)

  :config
  (pinentry-start))
