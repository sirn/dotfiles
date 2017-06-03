(req-package magit
  :require evil-leader
  :commands magit-status
  :init
  (evil-leader/set-key
    "gs" 'magit-status))

(req-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))
