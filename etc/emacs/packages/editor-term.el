;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature eshell
  :preface
  (eval-when-compile
    (declare-function gemacs--eshell-remove-pcomplete nil))

  :leader
  ("'" #'eshell))


(use-package multi-term
  :leader
  ("\"" #'multi-term))
