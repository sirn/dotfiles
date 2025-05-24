;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package gptel
  :defer 1

  :general
  (leader
    "gg" #'gemacs--gptel-transient-menu)

  :preface
  (eval-when-compile
    (declare-function gptel-api-key-from-auth-source nil))

  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'openai/gpt-4o-mini)
  (gptel-backend gemacs--gptel-openrouter-backend)

  :init
  (require 'transient)

  (defvar gemacs--gptel-openrouter-backend
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :key #'gptel-api-key-from-auth-source
      :stream t
      :models
      '(anthropic/claude-opus-4
        anthropic/claude-sonnet-4
        anthropic/claude-3.7-sonnet
        anthropic/claude-3.5-sonnet
        google/gemini-2.5-pro-preview
        google/gemini-2.5-flash-preview
        openai/gpt-4o-mini
        openai/gpt-4.1
        openai/gpt-4.1-mini)))

  (defvar gemacs--gptel-anthropic-backend
    (gptel-make-anthropic "Anthropic"
      :key #'gptel-api-key-from-auth-source
      :stream t
      :models
      '(claude-opus-4-20250514
        claude-sonnet-4-20250514
        claude-3-7-sonnet-20250219
        claude-3-5-haiku-20241022)))

  (defun gemacs--gptel-set-backend (backend &optional model)
    "Set the gptel backend to BACKEND and MODEL if provided."
    (setq gptel-backend backend)
    (when model
      (setq gptel-model model))
    (message "GPTel backend set to %s, model: %s"
             (gptel-backend-name backend)
             gptel-model))

  (transient-define-prefix gemacs--gptel-backend-menu ()
    "Select GPTel backend."
    ["Select Backend"
     ("o" "OpenRouter"
      (lambda ()
        (interactive)
        (gemacs--gptel-set-backend
         gemacs--gptel-openrouter-backend
         'openai/gpt-4o-mini)))
     ("a" "Anthropic"
      (lambda ()
        (interactive)
        (gemacs--gptel-set-backend
         gemacs--gptel-anthropic-backend
         'claude-sonnet-4-20250514)))])

  (transient-define-prefix gemacs--gptel-transient-menu ()
    "GPTel commands."
    ["GPTel Commands"
     ("o" "Open GPTel" gptel)
     ("s" "Send message" gptel-send)]
    ["Configuration"
     ("p" "System prompt" gptel-system-prompt)
     ("m" "Menu" gptel-menu)
     (";" "Switch backend" gemacs--gptel-backend-menu)])

  (defun gemacs--gptel-initialize-buffer ()
    (visual-line-mode t))

  (add-hook 'gptel-mode-hook #'gemacs--gptel-initialize-buffer))


(use-package aidermacs
  :defer t

  :general
  (leader
    "ga" #'aidermacs-transient-menu)

  :preface
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

  :config
  ;; Set environment variables for API keys if they exist in auth-source
  (dolist (api-config '(("openrouter.ai" . "OPENROUTER_API_KEY")
                        ("api.anthropic.com" . "ANTHROPIC_API_KEY")))
    (when-let ((key (auth-source-pick-first-password :host (car api-config) :user "apikey")))
      (setenv (cdr api-config) key)))

  (setq aidermacs-chat-completion-function 'aidermacs-chat-completion-with-gptel))
