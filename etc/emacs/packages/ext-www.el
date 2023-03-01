;; -*- lexical-binding: t; no-native-compile: t -*-

;; Nix is managing w3m due to dependency on w3m binary.
(gemacs-when-compiletime (locate-library "w3m")
  (use-feature w3m
    :general
    (leader
      ";W" #'w3m
      ";w" #'w3m-goto-url
      ";s" #'w3m-search)

    :custom
    (mm-text-html-renderer #'w3m)
    (w3m-search-default-engine "duckduckgo")
    (w3m-search-engine-alist
      '(("duckduckgo" "https://duckduckgo.com/lite?q=%s" utf-8)))))


(use-feature emacs
  :config
  (if (eq system-type 'darwin)
    (progn
      (setq browse-url-browser-function #'browse-url-generic)
      (setq browse-url-generic-program "open"))))
