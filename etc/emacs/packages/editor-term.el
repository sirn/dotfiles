;; -*- lexical-binding: t -*-

(use-feature eshell
  :commands eshell

  :leader
  ("'" #'eshell))


(use-package multi-term
  :commands multi-term

  :leader
  ("\"" #'multi-term))
