(use-package projectile
  :diminish projectile-mode
  :straight t

  :preface
  (eval-when-compile
    (defvar projectile-project-root-files)
    (declare-function projectile-mode nil)
    (declare-function projectile-invalidate-cache nil))

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "pk" 'projectile-kill-buffers))

  :config
  (projectile-mode t)

  (with-eval-after-load 'magit-branch
    (defun gr/projectile-invalidate-cache-adv (&rest _args)
      (projectile-invalidate-cache nil))

    (advice-add 'magit-checkout :after 'gr/projectile-invalidate-cache-adv)
    (advice-add 'magit-branch-and-checkout :after 'gr/projectile-invalidate-cache-adv)))
