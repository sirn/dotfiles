(req-package js2-mode
  :mode "\\.js\\'")

(req-package js2-refactor
  :require js2-mode
  :diminish js2-refactor-mode
  :commands js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(req-package json-mode
  :mode "\\.json\\'")

(req-package skewer-mode
  :require js2-mode
  :diminish skewer-mode
  :commands skewer-mode
  :init
  (progn
    (add-hook 'js2-mode-hook 'skewer-mode)))
