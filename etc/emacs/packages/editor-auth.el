;; -*- lexical-binding: t; no-native-compile: t -*-

;; Builtin
(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))


(use-package pinentry
  :demand t

  :config
  ;; Allow gpg-connect-agent in ssh-agent mode to forward pinentry to Emacs
  ;; since the ssh-agent protocol has no way to pass the TTY to gpg-agent.
  ;; See also --enable-ssh-support in gpg-agent(1)
  ;;
  ;; gpg-agent use INSIDE_EMACS environment variable to detect that we're
  ;; running in Emacs, but the environment variable is set in term-mode.
  ;; We do it here so all pinentry actually goes to Emacs.
  (setenv "INSIDE_EMACS" emacs-version)

  (defun gemacs--gpg-update-tty (&rest _args)
    (shell-command
     "gpg-connect-agent --no-autostart updatestartuptty /bye"
     " *gpg-update-tty*"))

  (pinentry-start))


(use-package auth-source
  :demand t

  :config
  (setq auth-sources `(,(no-littering-expand-etc-file-name "authinfo.gpg")
                        ,(expand-file-name "~/.authinfo.gpg")
                        ,(expand-file-name "~/.netrc"))))
