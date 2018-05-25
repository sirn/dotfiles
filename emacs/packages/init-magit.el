(req-package evil-magit
  :require (evil magit))

(req-package magit
  :require evil-leader
  :commands magit-status
  :diminish (auto-revert-mode magit-auto-revert-mode)
  :init
  (evil-leader/set-key
    "gs" 'magit-status)
  :config
  (progn
    (setq magit-push-current-set-remote-if-missing t)))
