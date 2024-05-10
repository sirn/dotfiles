 ;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package dired-sidebar
  :general
  (leader
    "tt" #'dired-sidebar-toggle-sidebar
    "tT" #'dired-sidebar-toggle-with-current-directory))
