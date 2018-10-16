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
  :after epa

  :preface
  (declare-function pinentry-start nil)

  :config
  (pinentry-start))
