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
    (defvar color-15)
    (declare-function gr/echo-area-setup-face nil))

  :init
  (setq tao-theme-use-boxes nil)
  (setq tao-theme-use-sepia nil)

  :config
  (load-theme 'tao-yin t)
  (tao-with-color-variables
    tao-theme-yin-palette
    (custom-theme-set-faces
     'tao-yin

     ;;
     ;; Custom faces for ansible
     ;;
     `(ansible::task-label-face ((t (:foreground ,color-9))))
     `(ansible::section-face    ((t (:foreground ,color-13 :weight bold))))

     ;;
     ;; Custom faces flycheck
     ;;
     `(flycheck-fringe-error    ((t (:foreground "red"))))
     `(flycheck-fringe-warning  ((t (:foreground "darkorange"))))
     `(flycheck-fringe-info     ((t (:foreground "green"))))
     `(flycheck-error           ((t (:underline t :inherit flycheck-fringe-error))))
     `(flycheck-warning         ((t (:underline t :inherit flycheck-fringe-warning))))
     `(flycheck-info            ((t (:underline t :inherit flycheck-fringe-info))))

     ;;
     ;; Custom faces for git-gutter
     ;;
     `(git-gutter:added         ((t (:foreground "green"))))
     `(git-gutter:deleted       ((t (:foreground "red"))))
     `(git-gutter:modified      ((t (:foreground "magenta"))))
     `(git-gutter:unchanged     ((t (:foreground "yellow"))))

     ;;
     ;; Custom faces for smart-mode-line
     ;;
     `(mode-line                ((t (:background ,color-3 :inherit default))))
     `(mode-line-buffer-id      ((t (:inherit sml/filename))))
     `(mode-line-inactive       ((t (:background ,color-6))))
     `(sml/global               ((t (:background nil :inherit mode-line))))
     `(sml/modes                ((t (:foreground ,color-8))))
     `(sml/minor-modes          ((t (:foreground ,color-8))))
     `(sml/read-only            ((t (:foreground ,color-8))))

     ;; Prefix
     ;; mode-line-winum/mode-line-evil
     `(gr/mode-line-winum       ((t (:foreground ,color-8 :weight bold))))
     `(gr/mode-line-evil        ((t (:foreground ,color-13 :weight bold))))
     `(gr/mode-line-notmuch     ((t (:foreground "cyan" :weight bold))))

     ;; Line number
     ;; line-number:col-number
     `(sml/col-number           ((t (:foreground ,color-8))))
     `(sml/line-number          ((t (:foreground ,color-10 :weight bold))))

     ;; Filename
     ;; [prefix(sudo/git/projectile)]folder/filename
     `(sml/prefix               ((t (:foreground ,color-15))))
     `(sml/sudo                 ((t (:foreground "red" :inherit sml/prefix))))
     `(sml/git                  ((t (:inherit sml/prefix))))
     `(sml/projectile           ((t (:inherit sml/prefix))))
     `(sml/folder               ((t (:foreground ,color-10))))
     `(sml/filename             ((t (:foreground ,color-10 :weight bold))))

     ;;
     ;; Custom faces for web-mode
     ;;
     `(web-mode-block-attr-name-face ((t (:foreground ,color-13 :weight bold)))))))
