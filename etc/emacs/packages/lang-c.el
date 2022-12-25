;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature cc-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'cc-mode-hook #'apheleia-mode))

  (use-feature flycheck
    :demand t

    :config
    (add-hook 'cc-mode-hook #'flycheck-mode)))
