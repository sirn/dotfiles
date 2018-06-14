(use-package projectile
  :diminish projectile-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function projectile-mode nil))

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "pk" 'projectile-kill-buffers))

  :config
  (projectile-mode t))
