;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package php-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'php-mode-hook #'apheleia-mode))

  (use-feature flymake
    :demand t

    :config
    (add-hook 'php-mode-hook #'flymake-mode)))
