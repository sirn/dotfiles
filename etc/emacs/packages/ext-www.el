;; -*- lexical-binding: t -*-

(use-package w3m
  :commands (w3m w3m-browse-url)

  :preface
  (eval-when-compile
    (defvar w3m-search-engine-alist)
    (defvar w3m-search-default-engine))

  :init
  (setq browse-url-browser-function #'w3m-browse-url)
  (setq mm-text-html-renderer #'w3m)

  :config
  (setq w3m-search-default-engine "duckduckgo")
  (setq w3m-search-engine-alist
    '(("duckduckgo" "https://duckduckgo.com/lite?q=%s" utf-8))))
