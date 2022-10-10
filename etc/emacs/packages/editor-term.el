;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature eshell
  :general
  (leader
    "'" #'eshell))


(use-package multi-term
  :general
  (leader
    "\"" #'multi-term))
