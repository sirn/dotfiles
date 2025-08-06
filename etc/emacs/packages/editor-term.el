;; -*- lexical-binding: t; no-native-compile: t -*-

(defun gemacs--term-setup ()
  (setq-local evil-insert-state-cursor 'box)
  (evil-insert-state))


(defun gemacs--term-with-editor-setup ()
  (unless (string-match-p "\\*claude-code\\[.*\\]\\*" (buffer-name)
            (with-editor-export-editor))))


;; Builtin
(use-package term
  :init
  (with-eval-after-load 'with-editor
    (add-hook 'term-mode-hook #'with-editor-export-editor))

  :config
  (add-hook 'term-mode-hook #'gemacs--term-setup)
  (add-hook 'term-mode-hook #'gemacs--term-with-editor-setup))


;; Builtin
(use-package eshell
  :general
  (leader
    "'e" #'eshell)

  :init
  (with-eval-after-load 'with-editor
    (add-hook 'eshell-mode-hook #'with-editor-export-editor))

  :config
  (add-hook 'eshell-mode-hook #'gemacs--term-setup)
  (add-hook 'eshell-mode-hook #'gemacs--term-with-editor-setup))


(use-package eat
  :general
  (leader
    "'v" #'eat
    "''" #'gemacs--eat-project)

  :custom
  (eat-term-name "xterm-256color")

  :preface
  (eval-when-compile
    (declare-function eat nil)
    (declare-function project-current nil)
    (declare-function project-root nil))

  :init
  (defun gemacs--eat-project ()
    "Start eat terminal in project root, or current directory if no project."
    (interactive)
    (let ((default-directory
           (if-let ((project (project-current)))
             (project-root project)
             default-directory)))
      (eat)))

  :config
  ;; Note: with-editor doesn't support eat just yet
  ;; See also https://github.com/magit/with-editor/discussions/128
  (add-hook 'eat-mode-hook #'gemacs--term-setup))
