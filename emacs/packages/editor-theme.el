(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))


(use-package tao-theme
  :ensure t

  :preface
  (eval-when-compile
    (defvar color-0)
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

      ;; helm-rg
      `(helm-rg-preview-line-highlight ((t (:foreground ,color-10))))
      `(helm-rg-base-rg-cmd-face       ((t (:foreground ,color-8))))
      `(helm-rg-inactive-arg-face      ((t (:foreground ,color-8))))
      `(helm-rg-active-arg-face        ((t (:foreground ,color-13))))
      `(helm-rg-directory-cmd-face     ((t (:foreground ,color-10))))
      `(helm-rg-error-message          ((t (:foreground ,color-10))))
      `(helm-rg-title-face             ((t (:foreground ,color-10))))
      `(helm-rg-directory-header-face  ((t (:foreground ,color-10))))
      `(helm-rg-file-match-face        ((t (:foreground ,color-11 :weight bold))))
      `(helm-rg-colon-separator-ripgrep-output-face ((t (:foreground ,color-8))))
      `(helm-rg-line-number-match-face ((t (:foreground ,color-7))))
      `(helm-rg-match-text-face        ((t (:foreground ,color-10))))

      ;; web-mode
      `(web-mode-block-attr-name-face ((t (:foreground ,color-13 :wegiht bold)))))))
