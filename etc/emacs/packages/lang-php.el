;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package php-mode
  :init
  (add-hook 'php-mode-hook #'eglot-ensure)
  (add-hook 'php-mode-hook #'flycheck-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
      `(php-mode . ,(eglot-alternatives
                      `(("intelephense" "--stdio"
                          :initializationOptions
                          (:licenceKey ,(expand-file-name "~/.config/intelephense/licence.txt")))
                        ("phpactor" "language-server")))))))
