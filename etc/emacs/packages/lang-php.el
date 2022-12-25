;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package php-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'php-mode-hook #'apheleia-mode))

  (use-feature flycheck
    :demand t

    :config
    (add-hook 'php-mode-hook #'flycheck-mode)))
