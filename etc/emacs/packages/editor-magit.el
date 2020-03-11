;; -*- lexical-binding: t -*-

(use-package magit
  :commands magit-status

  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  (setq magit-remote-set-if-missing t)

  :config
  (use-feature pinentry
    :config
    (dolist (func '(magit-start-git magit-call-git))
      (advice-add func :after #'gemacs--gpg-update-tty)))

  (use-feature projectile
    :config
    (dolist (func '(magit-checkout magit-branch-and-checkout))
      (advice-add func :after #'gemacs--projectile-invalidate-cache)))

  :leader
  ("gs" #'magit-status))


(use-feature git-commit
  :config
  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))


(use-package forge)


(use-feature ghub
  :init

  ;; BUG: https://github.com/magit/ghub/issues/81
  (setq ghub-use-workaround-for-emacs-bug nil))


(use-feature emacsql-sqlite
  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql")))


(use-package git-gutter
  :init

  ;; BUG: https://github.com/syohex/emacs-git-gutter/issues/24
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

  (defun gemacs--git-gutter-load ()
    "Load `git-gutter' when initially finding a file."
    (require 'git-gutter)
    (remove-hook 'find-file-hook #'gemacs--git-gutter-load))
  (add-hook 'find-file-hook #'gemacs--git-gutter-load)

  :config
  (global-git-gutter-mode +1))
