(use-package projectile
  :defer 2
  :diminish projectile-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function projectile-mode nil)
    (declare-function projectile-invalid-cache nil))

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "pk" 'projectile-kill-buffers))

  :config
  (projectile-mode t)

  (with-eval-after-load 'magit-branch
    (defun projectile-invalid-cache-adv (&rest _args)
      (projectile-invalidate-cache nil))
    (advice-add 'magit-checkout :after 'projectile-invalidate-cache-adv)
    (advice-add 'magit-branch-and-checkout :after 'projectile-invalid-cache-adv)))
