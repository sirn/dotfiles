(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))


(use-package tao-theme
  :straight t

  :preface
  (eval-when-compile
    (defvar color-1)
    (defvar color-2)
    (defvar color-3)
    (defvar color-4)
    (defvar color-5)
    (defvar color-6)
    (defvar color-7)
    (defvar color-8)
    (defvar color-9)
    (defvar color-10)
    (defvar color-11)
    (defvar color-12)
    (defvar color-13)
    (defvar color-14)
    (defvar color-15))

  :config
  (load-theme 'tao-yin t)
  (tao-with-color-variables tao-theme-yin-palette
   (custom-theme-set-faces
      'tao-yin

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
      `(web-mode-block-attr-name-face ((t (:foreground ,color-13 :weight bold)))))))
