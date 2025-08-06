;; -*- lexical-binding: t; no-native-compile: t -*-

(defun gemacs--term-setup ()
  (setq-local evil-insert-state-cursor 'box)
  (evil-insert-state))


(defun gemacs--term-with-editor-setup ()
  (unless (string-match-p "\\*claude-code\\[.*\\]\\*" (buffer-name))
    (with-editor-export-editor)))


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
    "''" #'eat)

  :preface
  (eval-when-compile
    (declare-function eat nil)
    (declare-function project-current nil)
    (declare-function project-root nil))

  :config
  (setq eat-term-name "xterm-256color")
  (setq eat-kill-buffer-on-exit t)

  (add-hook 'eat-mode-hook #'gemacs--term-setup)

  ;; Note: with-editor doesn't support eat just yet
  ;; See also https://github.com/magit/with-editor/discussions/128
  (cl-defun with-editor-export-editor-eat (process &optional (envvar "EDITOR"))
    "Like `with-editor-export-editor', but for `eat-exec-hook'."
    (cond
     ((derived-mode-p 'eat-mode)
      (if with-editor-emacsclient-executable
          (let ((with-editor--envvar envvar)
                (process-environment process-environment))
            (with-editor--setup)
            (while (accept-process-output process 0.1))
            (when-let ((v (getenv envvar)))
              (eat-term-send-string eat-terminal (format " export %s=%S" envvar v))
              (eat-self-input 1 'return))
            (when-let ((v (getenv "EMACS_SERVER_FILE")))
              (eat-term-send-string eat-terminnal (format " export EMACS_SERVER_FILE=%S" v))
              (eat-self-input 1 'return))
            (eat-term-send-string eat-terminal "clear")
            (eat-self-input 1 'return))
        (error "Cannot use sleeping editor in this buffer")))
     (t (error "Cannot export environment variables in this buffer")))
    (message "Successfully exported %s" envvar))

  (defun gemacs--eat-with-editor-setup (process)
    (unless (string-match-p "\\*claude-code\\[.*\\]\\*" (buffer-name))
      (with-editor-export-editor-eat process)))

  (add-hook 'eat-exec-hook #'gemacs--eat-with-editor-setup))
