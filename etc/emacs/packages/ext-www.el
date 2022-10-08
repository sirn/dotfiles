;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package w3m
  :preface
  (eval-when-compile
    (defvar w3m-search-engine-alist)
    (defvar w3m-search-default-engine))

  :general
  (leader
    ";W" #'w3m
    ";w" #'w3m-goto-url
    ";s" #'w3m-search)

  :custom
  (browse-url-browser-function #'w3m-browse-url)
  (mm-text-html-renderer #'w3m)
  (w3m-search-default-engine "duckduckgo")
  (w3m-search-engine-alist
    '(("duckduckgo" "https://duckduckgo.com/lite?q=%s" utf-8))))
