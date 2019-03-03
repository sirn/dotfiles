(use-package tramp
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq tramp-auto-save-directory "~/.cache/emacs/backups")
  (setq tramp-persistency-file-name "~/.emacs.d/data/tramp"))

(use-package docker-tramp
  :straight t)
