 ;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dired-sidebar
  :general
  (leader
    "TT" #'dired-sidebar-toggle-sidebar
    "Tt" #'dired-sidebar-toggle-with-current-directory)

  :config
  (setq dired-sidebar-theme 'ascii))
