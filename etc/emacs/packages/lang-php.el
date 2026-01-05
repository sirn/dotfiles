;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package php-mode
  :hook
  ((php-mode . eglot-ensure)
   (php-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
      `(php-mode . ,(eglot-alternatives
                      `(("intelephense" "--stdio"
                          :initializationOptions
                          (:licenceKey ,(expand-file-name "~/.config/intelephense/licence.txt")))
                        ("phpactor" "language-server")))))))
