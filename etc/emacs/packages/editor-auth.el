;; -*- lexical-binding: t; no-native-compile: t -*-

;; Builtin: GPG encryption interface
(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))


;; GPG PIN entry in Emacs minibuffer
(use-package pinentry
  :demand t

  :config
  (setenv "INSIDE_EMACS" emacs-version)

  (defun gemacs--gpg-update-tty (&rest _args)
    (shell-command
     "gpg-connect-agent --no-autostart updatestartuptty /bye"
     " *gpg-update-tty*"))

  (pinentry-start))


;; Builtin: secrets and credentials storage
(use-package auth-source
  :demand t

  :config
  (setq auth-sources `(,(no-littering-expand-etc-file-name "authinfo.gpg")
                        ,(expand-file-name "~/.authinfo.gpg")
                        ,(expand-file-name "~/.netrc"))))
