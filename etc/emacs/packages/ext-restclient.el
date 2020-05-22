;; -*- lexical-binding: t -*-

(use-package restclient
  :demand t)


(use-package company-restclient
  :demand t
  :config
  (add-to-list 'company-backends 'company-restclient))
