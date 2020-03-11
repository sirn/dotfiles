;; -*- lexical-binding: t -*-

(use-package projectile
  :defer 1

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)

  :config
  (projectile-mode +1)
  (defun gemacs--projectile-invalidate-cache (&rest _args)
    (projectile-invalidate-cache nil))

  :leader
  ("pk" #'projectile-kill-buffers
   "pr" #'projectile-run-project
   "p'" #'projectile-run-eshell
   "p!" #'projectile-run-async-shell-command-in-root))
