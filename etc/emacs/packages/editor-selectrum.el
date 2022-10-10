;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package selectrum
  :straight (:host github :repo "radian-software/selectrum")

  :demand t

  :general
  ("C-x C-b" #'switch-to-buffer)
  (leader
    "bb"  #'switch-to-buffer
    "wbb" #'switch-to-buffer-other-window
    "dv"  #'describe-variable
    "df"  #'describe-function)

  :preface
  (eval-when-compile
    (declare-function selectrum-mode nil))

  :config
  (selectrum-mode +1))


(use-package prescient
  :demand t

  :custom
  (prescient-history-length 1000)

  :preface
  (eval-when-compile
    (defvar prescient-history-length)
    (declare-function prescient-persist-mode nil))

  :config
  (prescient-persist-mode +1)
  (use-feature emacs
    :init
    (setq completion-styles '(prescient basic))))


(use-package selectrum-prescient
  :straight (:host github
              :repo "radian-software/prescient.el"
              :files ("selectrum-prescient.el"))

  :after (selectrum prescient)

  :demand t

  :preface
  (eval-when-compile
    (declare-function selectrum-prescient-mode nil))

  :config
  (selectrum-prescient-mode +1))
