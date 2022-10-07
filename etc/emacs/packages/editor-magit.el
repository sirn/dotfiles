;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package magit
  :leader
  ("gs" #'magit-status)

  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  (setq magit-remote-set-if-missing t)

  :config
  (use-feature pinentry
    :config
    (dolist (func '(magit-start-git magit-call-git))
      (advice-add func :after #'gemacs--gpg-update-tty)))

  (use-feature forge
    :demand t))


(use-feature git-commit
  :config
  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))


(use-package forge
  :preface
  (eval-when-compile
    (defvar forge-add-default-bindings))

  :init

  ;; BUG: https://github.com/emacs-evil/evil-collection/issues/543
  (setq forge-add-default-bindings nil))


(use-feature ghub
  :init

  ;; BUG: https://github.com/magit/ghub/issues/81
  (setq ghub-use-workaround-for-emacs-bug 'force))


(use-feature emacsql-sqlite
  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql")))


(use-package git-gutter
  ;; Since our magit is defer-loaded, git-gutter need to wait for magit
  ;; to prevent `ad-handle-definition' warning due to `vc-revert' being
  ;; redefined.
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
