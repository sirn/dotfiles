(use-package evil-magit
  :after (evil magit)
  :straight t)


(use-package magit
  :commands magit-status
  :diminish (auto-revert-mode magit-auto-revert-mode)
  :straight t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "gs" 'magit-status))

  :config
  (setq magit-push-current-set-remote-if-missing t))


(use-package magithub
  :after magit
  :straight t

  :preface
  (eval-when-compile
    (defvar magithub-clone-default-directory)
    (declare-function magithub-feature-autoinject nil))

  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Dev/src/github.com/"))
