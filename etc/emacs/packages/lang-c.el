;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature cc-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'cc-mode-hook #'apheleia-mode))

  (use-feature flymake
    :demand t

    :config
    (add-hook 'cc-mode-hook #'flymake-mode)))
