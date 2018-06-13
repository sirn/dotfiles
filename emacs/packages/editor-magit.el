(use-package evil-magit
  :after (evil magit)
  :ensure t)


(use-package magit
  :commands magit-status
  :diminish (auto-revert-mode magit-auto-revert-mode)
  :ensure t

  :init
  (eval-after-load 'evil-leader
    (evil-leader/set-key
      "gs" 'magit-status))

  :config
  (setq magit-push-current-set-remote-if-missing t))
