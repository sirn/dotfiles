(use-package evil-magit
  :after (evil magit)
  :straight t)


(use-package magit
  :commands magit-status
  :diminish (auto-revert-mode magit-auto-revert-mode)
  :straight t

  :preface
  (eval-when-compile
    (defvar magit-status-sections-hook)
    (declare-function magithub-feature-autoinject nil)
    (declare-function magit-insert-un/tracked-files-1 nil))

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "gs" 'magit-status))

  :config
  (setq magit-push-current-set-remote-if-missing t)

  (defun magit-ignored-files ()
    (magit-git-items "ls-files" "--others" "--ignored" "--exclude-standard" "-z" "--directory"))

  (defun magit-insert-ignored-files ()
    (-when-let (files (magit-ignored-files))
      (magit-insert-section (ignored)
        (magit-insert-heading "Ignored files:")
        (magit-insert-un/tracked-files-1 files nil)
        (insert ?\n))))

  (add-to-list 'magit-status-sections-hook 'magit-insert-ignored-files t))


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
