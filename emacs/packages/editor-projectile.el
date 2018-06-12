(req-package projectile
  :require tramp
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-switch-project-action 'projectile-dired)
    (projectile-mode)))
