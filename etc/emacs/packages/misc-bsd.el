;; -*- lexical-binding: t; no-native-compile: t -*-

(gemacs-when-compiletime (eq system-type 'berkeley-unix)
  (setq dired-use-ls-dired nil))
