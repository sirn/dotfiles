;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature eshell
  :general
  (leader
    "'" #'eshell)

  :preface
  (eval-when-compile
    (declare-function gemacs--eshell-remove-pcomplete nil)))


(use-package multi-term
  :general
  (leader
    "\"" #'multi-term))
