;; -*- lexical-binding: t -*-

(use-package restclient
  :demand t)


(use-package company-restclient
  :demand t
  :config
  (add-to-list 'company-backends 'company-restclient))


(use-package ob-restclient
  :config
  (use-feature org
    :config
    (org-babel-do-load-languages
      'org-label-load-languages
      (append org-babel-load-languages
        '((restclient . t))))))
