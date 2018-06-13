(use-package projectile
  :after tramp
  :diminish projectile-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function projectile-mode nil))

  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (projectile-mode))
