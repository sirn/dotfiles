;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature eshell
  :preface
  (eval-when-compile
    (declare-function gemacs--eshell-remove-pcomplete nil))

  :general
  (leader
    "'" #'eshell))


(use-package multi-term
  :general
  (leader
    "\"" #'multi-term))
