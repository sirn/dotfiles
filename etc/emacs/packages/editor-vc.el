;; -*- lexical-binding: t; no-native-compile: t -*-

;; Git interface
(use-package magit
  :general
  (leader
    "g" #'magit-project-status)

  :custom
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-bind-magit-project-status nil)
  (magit-remote-set-if-missing t)

  :config
  (with-eval-after-load 'pinentry
    (dolist (func '(magit-start-git magit-call-git))
      (advice-add func :before #'gemacs--gpg-update-tty)))

  ;; Deferred loading
  (require 'forge))


;; Part of magit: commit message editing
(use-package git-commit
  :custom
  (git-commit-summary-max-length 50))


;; GitHub/GitLab/etc. API client
(use-package ghub)


;; GitHub/GitLab issues and PRs in magit
(use-package forge
  :preface
  (eval-when-compile
    (defvar forge-add-default-bindings))

  :init
  (setq forge-add-default-bindings nil))


;; Show git diff in the gutter
(use-package git-gutter
  :custom
  (git-gutter:disabled-modes '(fundamental-mode org-mode))

  :hook (after-init . global-git-gutter-mode))
