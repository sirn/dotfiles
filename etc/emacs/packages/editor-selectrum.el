;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package selectrum
  :straight (:host github :repo "radian-software/selectrum")

  :demand t

  :config
  (selectrum-mode +1))


(use-package prescient
  :demand t

  :custom
  (prescient-history-length 1000)

  :config
  (prescient-persist-mode +1)

  (use-feature emacs
    :custom
    (completion-styles '(prescient basic))))


(use-package selectrum-prescient
  :straight (:host github
              :repo "radian-software/prescient.el"
              :files ("selectrum-prescient.el"))

  :after (selectrum prescient)

  :demand t

  :config
  (selectrum-prescient-mode +1))
