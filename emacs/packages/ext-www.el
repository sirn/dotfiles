(use-package w3m
  :commands (w3m w3m-browse-url)
  :straight t

  :preface
  (eval-when-compile
    (defvar w3m-search-default-engine))

  :init
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq mm-text-html-renderer 'w3m)
  (setq w3m-search-default-engine "duckduckgo"))
