;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package gptel
  :defer 1

  :preface
  (eval-when-compile
    (declare-function gptel-mode nil)
    (declare-function auth-source-search nil)
    (declare-function gemacs--gptel-api-key-from-auth-source nil))

  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'claude-3-5-sonnet-20240620)

  :init
  (add-hook 'gptel-mode-hook #'visual-line-mode)

  :config
  (defun gemacs--gptel-api-key-from-auth-source (host)
    "Retrieve API key for HOST from auth-source."
    (when-let ((secret (plist-get (car (auth-source-search :host host :max 1)) :secret)))
      (if (functionp secret)
          (funcall secret)
        secret)))

  (let ((api-key (gemacs--gptel-api-key-from-auth-source "anthropic.com")))
    (setq gptel-backend (gptel-make-anthropic
                         "Claude Sonnet"
                         :key api-key
                         :stream t))))
