(use-package clojure-mode
  :straight t
  :mode "\\.clj\\'"

  :init
  (add-hook 'clojure-mode-hook 'flycheck-mode))
