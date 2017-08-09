(req-package magit
  :require evil-leader
  :diminish (auto-revert-mode magit-auto-revert-mode)
  :commands magit-status
  :init
  (evil-leader/set-key
    "gs" 'magit-status))

(req-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))
