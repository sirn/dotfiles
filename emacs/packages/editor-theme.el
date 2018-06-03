(defun custom/customize-theme ()
  (custom-theme-set-faces
   'tao-yin

   ;; emacs
   `(mode-line                ((,class (:foreground ,color-5 :background ,color-15))))
   `(mode-line-buffer-id      ((t (:foreground ,color-5))))

   ;; ansible
   `(ansible::task-label-face ((t (:foreground ,color-9))))
   `(ansible::section-face    ((t (:foreground ,color-13 :weight bold))))

   ;; flycheck
   `(flycheck-error           ((((supports :underline (:style wave))) (:underline (:style wave :color "Red1") :inherit unspecified)) (t (:foreground "Red1" :weight bold :underline t))))
   `(flycheck-warning         ((((supports :underline (:style wave))) (:underline (:style wave :color "DarkOrange") :inherit unspecified)) (t (:foreground "DarkOrange" :weight bold :underline t))))
   `(flycheck-info            ((((supports :underline (:style wave))) (:underline (:style wave :color "ForestGreen") :inherit unspecified)) (t (:foreground "ForestGreen" :weight bold :underline t))))
   `(flycheck-fringe-error    ((t (:foreground "Red1"))))
   `(flycheck-fringe-warning  ((t (:foreground "DarkOrange"))))
   `(flycheck-fringe-info     ((t (:foreground "ForestGreen"))))

   ;; git-gutter
   `(git-gutter:added         ((t (:foreground "green"))))
   `(git-gutter:deleted       ((t (:foreground "red"))))
   `(git-gutter:modified      ((t (:foreground "magenta"))))
   `(git-gutter:unchanged     ((t (:foreground "yellow"))))

   ;; web-mode
   `(web-mode-block-attr-name-face ((t (:foreground ,color-13 :wegiht bold))))))

(req-package tao-theme
  :config
  (progn
    (load-theme 'tao-yin t)
    (tao-with-color-variables tao-theme-yin-palette (custom/customize-theme))))
