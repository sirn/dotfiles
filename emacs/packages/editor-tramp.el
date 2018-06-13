(use-package tramp
  :defer 3
  :ensure f

  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups")
  (setq tramp-persistency-file-name "~/.emacs.d/data/tramp"))
