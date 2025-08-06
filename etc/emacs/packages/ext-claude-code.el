;; -*- lexical-binding: t; no-native-compile: t -*-

(gemacs-when-compiletime (executable-find "claude")
  (use-package claude-code-ide
    :defer t

    :general
    (leader
      "a c" #'claude-code-ide-menu)

    :config
    (claude-code-ide-emacs-tools-setup)

    :custom
    (claude-code-ide-window-side 'right)
    (claude-code-ide-focus-on-open t)
    (claude-code-ide-terminal-backend 'eat)))
