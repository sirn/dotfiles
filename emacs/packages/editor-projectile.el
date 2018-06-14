(use-package projectile
  :diminish projectile-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function projectile-mode nil))

  :init
  (setq projectile-switch-project-action 'projectile-dired)

  :config
  (projectile-mode t))
