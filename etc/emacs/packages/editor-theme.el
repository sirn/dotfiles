(use-package monokai-theme
  :straight t

  :config
  (load-theme 'monokai t)
  (custom-theme-set-faces
   'monokai
   '(transient-separator ((t :inherit background :foreground "#333333" :underline t)))))
