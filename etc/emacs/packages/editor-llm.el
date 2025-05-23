;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package gptel
  :defer 1

  :general
  (leader
    "gg" #'gptel
    "gP" #'gptel-system-prompt
    "gM" #'gptel-menu)

  :preface
  (eval-when-compile
    (declare-function gptel-api-key-from-auth-source nil))

  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'openai/gpt-4o-mini)
  (gptel-backend (gptel-make-openai "OpenRouter"
                   :host "openrouter.ai"
                   :endpoint "/api/v1/chat/completions"
                   :key #'gptel-api-key-from-auth-source
                   :stream t
                   :models
                   '(anthropic/claude-3.7-sonnet
                     anthropic/claude-3.5-sonnet
                     google/gemini-2.5-pro-preview
                     google/gemini-2.5-flash-preview
                     openai/gpt-4o-mini
                     openai/gpt-4.1
                     openai/gpt-4.1-mini)))

  :init
  (defun gemacs--gptel-initialize-buffer ()
    (display-line-numbers-mode -1)
    (display-fill-column-indicator-mode -1))

  (add-hook 'gptel-mode-hook #'gemacs--gptel-initialize-buffer))


(use-package aidermacs
  :defer t

  :general
  (leader
    "ga" #'aidermacs-transient-menu)

  :config
  (setq aidermacs-chat-completion-function 'aidermacs-chat-completion-with-gptel)

  (defun gemacs--aidermacs-project-root ()
    "Return the project root if in a project, otherwise `default-directory'."
    (if (project-current)
        (project-root (project-current))
      default-directory))
  (advice-add 'aidermacs-project-root :override #'gemacs--aidermacs-project-root)

  (defun gemacs--aidermacs-run-around (orig-fun &rest args)
    "Run `aidermacs-run' with `default-directory' set to project root."
    (let ((default-directory (gemacs--aidermacs-project-root)))
      (apply orig-fun args)))
  (advice-add 'aidermacs-run :around #'gemacs--aidermacs-run-around)

  (defun gemacs--aidermacs-initialize-buffer ()
    (display-line-numbers-mode -1)
    (display-fill-column-indicator-mode -1))

  (add-hook 'aidermacs-comint-mode-hook #'gemacs--aidermacs-initialize-buffer))
