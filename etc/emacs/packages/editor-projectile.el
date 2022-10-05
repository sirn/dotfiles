;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package projectile
  :defer 1

  :leader
  ("p/" #'projectile-ag
   "pf" #'projectile-find-file
   "pp" #'projectile-switch-project
   "pb" #'projectile-switch-to-buffer
   "pk" #'projectile-kill-buffers
   "pr" #'projectile-run-project
   "p'" #'projectile-run-eshell
   "p!" #'projectile-run-async-shell-command-in-root
   "pc" #'projectile-compile-project
   "pr" #'projectile-replace
   "pR" #'projectile-replace-regexp)

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'default)

  :config
  (projectile-mode +1)

  (defun gemacs--projectile-invalidate-cache (&rest _args)
    (projectile-invalidate-cache nil))

  ;; Integrating projectile with project.el
  (defun gemacs--projectile-project-find (dir)
    (when (projectile-project-p dir)
      (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root)))))

  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'gemacs--projectile-project-find)))
