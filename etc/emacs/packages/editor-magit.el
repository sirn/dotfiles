;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package magit
  :general
  (leader
    "gs" #'magit-project-status)

  :custom
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-bind-magit-project-status nil)
  (magit-remote-set-if-missing t)

  :init
  ;; magit binds project-status a bit too late; we're doing this by ourselves.
  (use-feature project
    :config
    (general-with-eval-after-load 'general
      (general-define-key :keymaps 'project-prefix-map "m" #'magit-project-status)
      (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))

  :config
  (use-feature pinentry
    :config
    (dolist (func '(magit-start-git magit-call-git))
      (advice-add func :after #'gemacs--gpg-update-tty)))

  (use-feature git-commit
    :custom
    ;; Max length for commit message summary is 50 characters as per
    ;; https://chris.beams.io/posts/git-commit/.
    (git-commit-summary-max-length 50)))


(use-package forge
  :demand t

  :after magit

  :defines forge-add-default-bindings

  :custom
  ;; BUG: https://github.com/emacs-evil/evil-collection/issues/543
  (forge-add-default-bindings nil)

  :config
  (use-feature ghub
    :custom
    ;; BUG: https://github.com/magit/ghub/issues/81
    (ghub-use-workaround-for-emacs-bug 'force))

  (use-feature emacsql-sqlite
    :custom
    ;; Put the EmacSQL binary in the repository, not the build dir. That
    ;; way we don't have to recompile it every time packages get rebuilt
    ;; by straight.el.
    (emacsql-sqlite-data-root (straight--repos-dir "emacsql"))))


(use-package git-gutter
  :custom
  ;; BUG: https://github.com/syohex/emacs-git-gutter/issues/24
  (git-gutter:disabled-modes '(fundamental-mode org-mode))

  ;; Since our magit is defer-loaded, git-gutter need to wait for magit
  ;; to prevent `ad-handle-definition' warning due to `vc-revert' being
  ;; redefined.
  :init
  (defun gemacs--git-gutter-load ()
    "Load `git-gutter' when initially finding a file."
    (require 'git-gutter)
    (remove-hook 'find-file-hook #'gemacs--git-gutter-load))
  (add-hook 'find-file-hook #'gemacs--git-gutter-load)

  :config
  (global-git-gutter-mode +1))
