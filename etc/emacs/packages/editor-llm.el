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
  (setq aidermacs-chat-completion-function 'aidermacs-chat-completion-with-gptel))
