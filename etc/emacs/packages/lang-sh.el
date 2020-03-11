;; -*- lexical-binding: t -*-

(use-feature sh-script
  :config
  (dolist (func '(sh-set-shell sh-make-vars-local))
    (advice-add func :around #'gemacs--advice-silence-messages))

  (defun gemacs--sh-prettify-mode-line ()
    "Instead of \"Shell[bash]\", display mode name as \"Bash\"."
    ;; Only do this for `sh-mode', not derived modes such as
    ;; `pkgbuild-mode'.
    (setq mode-line-process nil)
    (when (eq major-mode 'sh-mode)
      (setq mode-name (capitalize (symbol-name sh-shell)))))

  (add-hook 'sh-mode-hook #'gemacs--sh-prettify-mode-line))
