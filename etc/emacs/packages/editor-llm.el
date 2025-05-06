;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package gptel
  :defer 1

  :general
  (leader
    "gg" #'gptel)

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
                     google/gemini-2.5-pro-preview-03-25
                     google/gemini-2.5-flash-preview
                     openai/gpt-4o-mini)))

  :init
  (add-hook 'gptel-mode-hook #'visual-line-mode)

  :config
  (general-define-key :keymaps 'gptel-mode-map
    "C-c C-c" #'gptel-menu))
