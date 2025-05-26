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
  (with-eval-after-load 'project
    (general-with-eval-after-load 'general
      (general-define-key :keymaps 'project-prefix-map "m" #'magit-project-status)
      (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))

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
  ;; BUG: https://github.com/emacs-evil/evil-collection/issues/543
  (setq forge-add-default-bindings nil)

  :config
  (with-eval-after-load 'ghub
    ;; BUG: https://github.com/magit/ghub/issues/81
    (setq ghub-use-workaround-for-emacs-bug 'force)))


(use-package git-gutter
  :custom
  ;; BUG: https://github.com/syohex/emacs-git-gutter/issues/24
  (git-gutter:disabled-modes '(fundamental-mode org-mode))

  :preface
  (eval-when-compile
    (declare-function global-git-gutter-mode nil))

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
