(use-package tramp
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq tramp-auto-save-directory "~/.cache/emacs/backups")
  (setq tramp-persistency-file-name "~/.emacs.d/data/tramp"))
