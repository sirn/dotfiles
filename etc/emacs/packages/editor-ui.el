;; -*- lexical-binding: t -*-

;; Setup a minimalist frame without toolbar and menu bar, but also taking
;; OS behavior quirks into consideration. This is done via `make-frame-func'
;; to allow GUI `emacsclient' connecting to `emacs-server' to have different
;; frame settings independent of CLI ones.

(defvar gemacs-font "PragmataPro Mono")
(defvar gemacs-font-size 11)

(eval-when-compile
  (declare-function scroll-bar-mode nil)
  (declare-function mac-auto-operator-composition-mode nil))

(setq-default frame-title-format '("%f"))
(tool-bar-mode -1)
(menu-bar-mode -1)

(defun gemacs--after-make-frame-func (frame)
  "Setup frame attributes after a FRAME is created."
  (if (display-graphic-p frame)
    (let ((w (window-system frame)))
      (cond
        ((eq w 'x)
         (scroll-bar-mode -1)
         (set-frame-font (format "%s %s" gemacs-font gemacs-font-size)
           nil
           t))
        ((eq w 'mac)
         ;; macOS will "float" Emacs window if menu-bar-mode is disabled.
         ;; (e.g. not sticky to Spaces and no fullscreen support)
         (menu-bar-mode 1)
         (scroll-bar-mode -1)
         ;; macOS display font size about x1.2 smaller than other Unices.
         (set-frame-font (format "%s %s"
                           gemacs-font
                           (round (* gemacs-font-size 1.2)))
           nil
           t)
         (when (boundp 'mac-auto-operator-composition-mode)
           (mac-auto-operator-composition-mode)))))

    (progn
      (eval-and-compile
        (defun gemacs-scroll-down ()
          "Scroll down three lines."
          (interactive)
          (scroll-down 3))

        (defun gemacs-scroll-up ()
          "Scroll up three lines."
          (interactive)
          (scroll-up 3)))

      (xterm-mouse-mode t)
      (bind-key "<mouse-4>" #'gemacs-scroll-down)
      (bind-key "<mouse-5>" #'gemacs-scroll-up))))

(add-hook 'after-make-frame-functions #'gemacs--after-make-frame-func)
(add-hook 'after-init-hook
  `(lambda ()
     (gemacs--after-make-frame-func (selected-frame))))


(use-package winum
  :demand t

  :init
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local)

  :config
  (use-feature neotree
    :config
    (defun gemacs--winum-assign-func ()
      "Assign NeoTree window as window number 0."
      (when (and (boundp 'neo-buffer-name)
              (string= (buffer-name) neo-buffer-name)
              (eq (selected-window) (frame-first-window)))
        0))
    (add-to-list 'winum-assign-functions 'gemacs--winum-assign-func))

  (winum-mode +1)

  :leader
  ("0" #'winum-select-window-0
    "1" #'winum-select-window-1
    "2" #'winum-select-window-2
    "3" #'winum-select-window-3
    "4" #'winum-select-window-4
    "5" #'winum-select-window-5
    "6" #'winum-select-window-6
    "7" #'winum-select-window-7
    "8" #'winum-select-window-8
    "9" #'winum-select-window-9))


(use-package telephone-line)


(use-package tao-theme)


(defun gemacs--disable-theme ()
  "Disable theme propogation."
  (mapc #'disable-theme custom-enabled-themes))

(advice-add 'theme-dont-propagate :before #'gemacs--disable-theme)


;; Enable theme as late as is humanly possible. This reduces
;; frame flashing and other artifacts during startup.

(add-hook 'gemacs-after-init-hook
  `(lambda ()
     (use-feature telephone-line
       :demand t)

     (use-feature telephone-line-config
       :demand t

       :config
       (setq telephone-line-lhs
         '((nil . (telephone-line-window-number-segment))
           (evil . (telephone-line-evil-tag-segment))
           (accent . (telephone-line-vc-segment
                      telephone-line-erc-modified-channels-segment
                      telephone-line-process-segment))
           (nil . (telephone-line-projectile-segment
                   telephone-line-buffer-segment))))

       (telephone-line-mode +1))

     (use-feature tao-theme
       :demand t

       :init
       (setq tao-theme-use-boxes nil)
       (setq tao-theme-use-sepia nil)

       :config
       (load-theme 'tao-yin t))))
