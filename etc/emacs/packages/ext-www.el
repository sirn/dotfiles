;; -*- lexical-binding: t; no-native-compile: t -*-

;; Builtin
(use-package eww
  :general
  (leader
    "A w w" #'eww
    "A w s" #'eww-search-words
    "A w b" #'eww-list-bookmarks)

  :custom
  (eww-search-prefix "https://duckduckgo.com/lite?q="))


(use-package emacs
  :config
  (if (eq system-type 'darwin)
    (progn
      (setq browse-url-browser-function #'browse-url-generic)
      (setq browse-url-generic-program "open"))))
