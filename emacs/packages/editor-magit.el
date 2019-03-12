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
  :after magit)
