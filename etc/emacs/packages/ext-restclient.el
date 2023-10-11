;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package restclient
  :config
  (require 'jq-mode))


(use-package ob-restclient
  :preface
  (eval-when-compile
    (defvar org-babel-load-languages))

  :init
  (with-eval-after-load 'org
    (org-babel-do-load-languages
      'org-label-load-languages
      (append org-babel-load-languages
        '((restclient . t))))))
