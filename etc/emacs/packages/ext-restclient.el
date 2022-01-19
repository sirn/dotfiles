;; -*- lexical-binding: t -*-

(use-package restclient
  :config
  (use-feature jq-mode
    :demand t))


(use-package company-restclient
  :init
  (use-feature company
    :config
    (add-to-list 'company-backends 'company-restclient)))


(use-package ob-restclient
  :init
  (use-feature org
    :config
    (org-babel-do-load-languages
      'org-label-load-languages
      (append org-babel-load-languages
        '((restclient . t))))))
