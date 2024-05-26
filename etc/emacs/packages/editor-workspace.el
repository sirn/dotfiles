;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package perspective
  :demand t

  :general
  ("C-x k" #'persp-kill-buffer*)

  (leader
    "bd" #'persp-kill-buffer*
    "PP" #'persp-switch
    "Pn" #'persp-next
    "Pk" #'persp-kill)

  :preface
  (eval-when-compile
    (declare-function consult-customize nil)
    (declare-function persp-kill nil)
    (declare-function persp-kill-buffer* nil)
    (declare-function persp-mode nil)
    (declare-function persp-switch nil)
    (defvar persp-consult-source))

  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode +1)

  :config
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))
