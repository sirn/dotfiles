(req-package magit
  :require evil-leader
  :commands magit-status
  :diminish (auto-revert-mode magit-auto-revert-mode)
  :init
  (evil-leader/set-key
    "gs" 'magit-status))

(req-package evil-magit
  :require (evil magit))

(req-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))
