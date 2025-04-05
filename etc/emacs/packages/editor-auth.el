;; -*- lexical-binding: t; no-native-compile: t -*-

;; Builtin
(use-package epg
  :custom
  (epg-pinentry-mode 'loopback))


(use-package auth-source)
  :demand t

  :config
  (setq auth-sources `(,(no-littering-expand-etc-file-name "authinfo.gpg")
                       ,(expand-file-name "~/.authinfo.gpg")
                       ,(expand-file-name "~/.netrc")))
