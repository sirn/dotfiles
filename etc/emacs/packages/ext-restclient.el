;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package restclient
  :config
  (use-feature jq-mode
    :demand t))


(use-package ob-restclient
  :init
  (use-feature org
    :config
    (org-babel-do-load-languages
      'org-label-load-languages
      (append org-babel-load-languages
        '((restclient . t))))))
