;; -*- lexical-binding: t; no-native-compile: t -*-


(defun gemacs--llm-env-from-auth-source ()
  (dolist (api-config '(("openrouter.ai" . "OPENROUTER_API_KEY")
                        ("api.anthropic.com" . "ANTHROPIC_API_KEY")
                        ("generativelanguage.googleapis.com" . "GEMINI_API_KEY")
                        ("api.openai.com" . "OPENAI_API_KEY")))
    (when-let ((key (auth-source-pick-first-password :host (car api-config) :user "apikey")))
      (setenv (cdr api-config) key))))


(use-package gptel
  :defer 1

  :general
  (leader
    "a g" #'gemacs--gptel-transient-menu)

  :preface
  (eval-when-compile
    (declare-function gptel-api-key-from-auth-source nil))

  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'gemini-2.5-pro)

  :init
  (require 'transient)

  (defvar gemacs--gptel-anthropic-backend
    (gptel-make-anthropic "Anthropic"
      :key #'gptel-api-key-from-auth-source
      :stream t
      :models
      '(claude-opus-4-0
        claude-sonnet-4-0
        claude-3-7-sonnet-latest
        claude-3-5-haiku-latest)))

  (defvar gemacs--gptel-gemini-backend
    (gptel-make-gemini "Gemini"
      :key #'gptel-api-key-from-auth-source
      :stream t
      :models
      '(gemini-2.5-pro
        gemini-2.5-flash)))

  (defvar gemacs--gptel-openai-backend
    (gptel-make-openai "OpenAI"
      :key #'gptel-api-key-from-auth-source
      :stream t
      :models
      '(o3-pro
        o3
        o4-mini
        gpt-4.1)))

  (defvar gemacs--gptel-openrouter-backend
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :key #'gptel-api-key-from-auth-source
      :stream t
      :models
      '(meta-llama/llama-4-maverick
        meta-llama/llama-4-scout)))

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
     ("a" "Anthropic"
      (lambda ()
        (interactive)
        (gemacs--gptel-set-backend
         gemacs--gptel-anthropic-backend
         'claude-sonnet-4-0)))
     ("g" "Gemini"
      (lambda ()
        (interactive)
        (gemacs--gptel-set-backend
         gemacs--gptel-gemini-backend
         'gemini-2.5-flash)))
     ("o" "OpenAI"
      (lambda ()
        (interactive)
        (gemacs--gptel-set-backend
         gemacs--gptel-openai-backend
         'o3)))
     ("r" "OpenRouter"
      (lambda ()
        (interactive)
        (gemacs--gptel-set-backend
         gemacs--gptel-openrouter-backend
         'meta-llama/llama-4-maverick)))])

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

  (add-hook 'gptel-mode-hook #'gemacs--gptel-initialize-buffer)

  (setq gptel-backend gemacs--gptel-gemini-backend)

  :config
  (gemacs--llm-env-from-auth-source))


(use-package aidermacs
  :defer t

  :general
  (leader
    "a a" #'aidermacs-transient-menu)

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
  (gemacs--llm-env-from-auth-source)
  (setq aidermacs-chat-completion-function 'aidermacs-chat-completion-with-gptel))
