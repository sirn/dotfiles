(use-package evil-magit
  :after (evil magit)
  :straight t)


(use-package magit
  :diminish (auto-revert-mode magit-auto-revert-mode)
  :straight t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "gs" 'magit-status))

  :config
  (setq magit-remote-set-if-missing t))


(use-package forge
  :straight t
  :after magit

  :config

  ;; BUG: https://github.com/magit/ghub/issues/81
  (setq ghub-use-workaround-for-emacs-bug nil)
  (when (< emacs-major-version 27)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))
