;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :demand t

  :preface
  (eval-when-compile
    (declare-function selectrum-mode nil))

  :bind
  (("C-x C-b" . #'switch-to-buffer))

  :leader
  ("bb"  #'switch-to-buffer
   "wbb" #'switch-to-buffer-other-window
   "dv"  #'describe-variable
   "df"  #'describe-function)

  :config
  (selectrum-mode +1))


(use-package prescient
  :demand t

  :preface
  (eval-when-compile
    (defvar prescient-history-length)
    (declare-function prescient-persist-mode nil))

  :init
  (setq prescient-history-length 1000)

  :config
  (prescient-persist-mode +1))


(use-package selectrum-prescient
  :straight (:host github
              :repo "raxod502/prescient.el"
              :files ("selectrum-prescient.el"))

  :after (selectrum prescient)
  :demand t

  :preface
  (eval-when-compile
    (declare-function selectrum-prescient-mode nil))

  :config
  (selectrum-prescient-mode +1))
