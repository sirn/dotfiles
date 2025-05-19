;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package gptel
  :defer 1

  :general
  (leader
    "gg" #'gptel
    "gp" #'gptel-system-prompt
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
  (add-hook 'gptel-mode-hook #'visual-line-mode))

(use-package aidermacs
  :defer t

  :general
  (leader
    "ma" #'aidermacs-transient-menu)

  :config
  (defun gemacs--aidermacs-project-root ()
    "Return the project root if in a project, otherwise `default-directory'."
    (if (project-current)
        (project-root (project-current))
      default-directory))
  (advice-add 'aidermacs-project-root :override #'gemacs--aidermacs-project-root)

  (defun gemacs--around-aidermacs-run (orig-fun &rest args)
    "Run `aidermacs-run' with `default-directory' set to project root."
    (let ((default-directory (gemacs--aidermacs-project-root)))
      (apply orig-fun args)))
  (advice-add 'aidermacs-run :around #'gemacs--around-aidermacs-run)

  (setq aidermacs-chat-completion-function 'aidermacs-chat-completion-with-gptel))
