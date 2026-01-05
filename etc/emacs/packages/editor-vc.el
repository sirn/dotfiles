;; -*- lexical-binding: t; no-native-compile: t -*-

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


(use-package git-commit
  :custom
  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (git-commit-summary-max-length 50))


(use-package ghub)


(use-package forge
  :preface
  (eval-when-compile
    (defvar forge-add-default-bindings))

  :init
  ;; Disable default bindings as evil-collection provides its own.
  (setq forge-add-default-bindings nil))


(use-package git-gutter
  :custom
  (git-gutter:disabled-modes '(fundamental-mode org-mode))

  :hook (after-init . global-git-gutter-mode))
