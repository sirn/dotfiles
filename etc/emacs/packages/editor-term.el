;; -*- lexical-binding: t; no-native-compile: t -*-


(defun gemacs--term-setup ()
  "Setup initial terminal state."
  (setq-local evil-insert-state-cursor 'box)
  (evil-insert-state))


(defun gemacs--term-with-editor-setup ()
  "Setup `with-editor' for terminal buffers."
  (with-eval-after-load 'with-editor
    (with-editor-export-editor)))


;; Builtin: basic terminal emulator
(use-package term
  :hook
  ((term-mode . gemacs--term-setup)
   (term-mode . gemacs--term-with-editor-setup)))


;; Builtin: Emacs Lisp shell
(use-package eshell
  :general
  (leader
    "'e" #'eshell)

  :hook
  ((eshell-mode . gemacs--term-setup)
   (eshell-mode . gemacs--term-with-editor-setup)))


;; Fast terminal emulator using Emacs Lisp
(use-package eat
  :general
  (leader
    "''" #'eat)

  :preface
  (eval-when-compile
    (declare-function eat nil)
    (declare-function project-current nil)
    (declare-function project-root nil))

  :hook
  ((eat-mode . gemacs--term-setup)
   (eat-exec . gemacs--eat-with-editor-setup))

  :config
  (setq eat-term-name "xterm-256color")
  (setq eat-kill-buffer-on-exit t)

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
              (eat-term-send-string eat-terminal (format " export EMACS_SERVER_FILE=%S" v))
              (eat-self-input 1 'return))
            (eat-term-send-string eat-terminal "clear")
            (eat-self-input 1 'return))
        (error "Cannot use sleeping editor in this buffer")))
     (t (error "Cannot export environment variables in this buffer")))
    (message "Successfully exported %s" envvar))

  (defun gemacs--eat-with-editor-setup (process)
    (with-editor-export-editor-eat process)))


;; Fast terminal emulator using libvterm
(use-package vterm
  :general
  (leader
    "'v" #'vterm)

  :hook
  ((vterm-mode . gemacs--term-setup)
   (vterm-mode . gemacs--term-with-editor-setup)))
