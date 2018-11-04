(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))


(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-city-lights t))
