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

     `(warning                  ((t (:foreground "DarkOrange" :weight bold))))
     `(error                    ((t (:foreground "Red1" :weight bold))))

     ;; Custom faces for ansible
     ;;
     `(ansible::task-label-face ((t (:foreground ,color-9))))
     `(ansible::section-face    ((t (:foreground ,color-13 :weight bold))))

     ;; Custom faces for flycheck
     `(flycheck-error           ((t (:foreground "Red1" :weight bold :underline t))))
     `(flycheck-warning         ((t (:foreground "DarkOrange" :weight bold :underline t))))
     `(flycheck-info            ((t (:foreground "ForestGreen" :weight bold :underline t))))
     `(flycheck-fringe-error    ((t (:foreground "Red1"))))
     `(flycheck-fringe-warning  ((t (:foreground "DarkOrange"))))
     `(flycheck-fringe-info     ((t (:foreground "ForestGreen"))))

     ;; Custom faces for mode-line and smart-mode-line
     ;; [prefix(sudo/git/projectile)]folder/filename
     ;;
     `(mode-line                ((t (:background ,color-5 :inherit default))))
     `(sml/prefix               ((t (:foreground ,color-15))))
     `(sml/sudo                 ((t (:foreground "Red" :inherit sml/prefix))))
     `(sml/git                  ((t (:inherit sml/prefix))))
     `(sml/projectile           ((t (:inherit sml/prefix))))
     `(sml/folder               ((t (:foreground ,color-10))))
     `(sml/filename             ((t (:foreground ,color-10 :weight bold))))

     ;; Custom faces for web-mode
     ;;
     `(web-mode-block-attr-name-face ((t (:foreground ,color-13 :weight bold)))))))
