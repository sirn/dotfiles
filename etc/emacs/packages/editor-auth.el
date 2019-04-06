(use-package auth-source
  :straight t

  :preface
  (eval-when-compile
    (defvar auth-sources))

  :config
  (setq auth-sources '()))


(use-package auth-source-pass
  :after (auth-source password-store)
  :straight t

  :preface
  (eval-when-compile
    (declare-function auth-source-pass-enable nil))

  :config
  (let ((dir (password-store-dir)))
    (when (file-directory-p dir)
      (auth-source-pass-enable))))


(use-package password-store
  :straight t)


(use-package pinentry
  :straight t

  :config

  ;; Allow gpg-connect-agent in ssh-agent mode to forward pinentry to Emacs
  ;; since the ssh-agent protocol has no way to pass the TTY to gpg-agent.
  ;; See also --enable-ssh-support in gpg-agent(1)
  ;;
  ;; gpg-agent use INSIDE_EMACS environment variable to detect that we're
  ;; running in Emacs, but the environment variable is set in term-mode.
  ;; We do it here so all pinentry actually goes to Emacs.
  ;;
  ;; Also this hook has a nice effect of auto-starting gpg-agent when
  ;; needed by ssh.
  (setenv "INSIDE_EMACS" emacs-version)

  (defun gr/gpg-update-tty (&rest _args)
    (shell-command
     "gpg-connect-agent updatestartuptty /bye"
     " *gpg-update-tty*"))

  (with-eval-after-load 'magit
    (advice-add 'magit-start-git :before 'gr/gpg-update-tty)
    (advice-add 'magit-call-git :before 'gr/gpg-update-tty)))


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
  :after epa

  :preface
  (declare-function pinentry-start nil)

  :config
  (pinentry-start))
