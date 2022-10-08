;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package winum
  :demand t

  :general
  (leader
    "0" #'winum-select-window-0
    "1" #'winum-select-window-1
    "2" #'winum-select-window-2
    "3" #'winum-select-window-3
    "4" #'winum-select-window-4
    "5" #'winum-select-window-5
    "6" #'winum-select-window-6
    "7" #'winum-select-window-7
    "8" #'winum-select-window-8
    "9" #'winum-select-window-9)

  :custom
  (winum-auto-setup-mode-line nil)
  (winum-scope 'frame-local)

  :config
  (winum-mode +1))


(use-package which-key
  :demand t

  :preface
  (eval-when-compile
    (declare-function which-key-mode nil))

  :config
  (which-key-mode +1))



(use-package telephone-line
  :preface
  (eval-when-compile
    (declare-function telephone-line-mode nil)
    (defvar flymake-mode-line-format nil)
    (defvar telephone-line-height)
    (defvar telephone-line-lhs)
    (defvar telephone-line-primary-left-separator)
    (defvar telephone-line-primary-right-separator)
    (defvar telephone-line-rhs)
    (defvar telephone-line-secondary-left-separator)
    (defvar telephone-line-secondary-right-separator))

  :custom
  (telephone-line-height 18)
  (telephone-line-primary-right-separator 'telephone-line-utf-abs-right)
  (telephone-line-secondary-right-separator 'telephone-line-utf-abs-hollow-right)
  (telephone-line-primary-left-separator 'telephone-line-utf-abs-left)
  (telephone-line-secondary-left-separator 'telephone-line-utf-abs-hollow-left)
  (telephone-line-lhs
    '((accent . (telephone-line-window-number-segment))
      (evil   . (telephone-line-evil-tag-segment))
      (nil    . (telephone-line-vc-segment))
      (nil    . (telephone-line-project-segment))
      (nil    . (telephone-line-process-segment
                 telephone-line-buffer-segment))))
  (telephone-line-rhs
    '((nil    . (gemacs--telephone-line-flymake-segment))
      (nil    . (telephone-line-misc-info-segment))
      (nil    . (telephone-line-major-mode-segment))
      (evil   . (telephone-line-airline-position-segment))))

  :config
  (telephone-line-defsegment* gemacs--telephone-line-flymake-segment ()
    (when (bound-and-true-p flymake-mode)
      (ignore face)  ;; silent "unused lexical argument"
      (telephone-line-raw
        (if (boundp 'flymake--mode-line-format)
          flymake--mode-line-format
          flymake-mode-line-format)
        t)))

  (telephone-line-mode +1))


(use-package modus-themes
  :preface
  (eval-when-compile
    (defvar modus-themes-mode-line))

  :custom
  (modus-themes-mode-line '(borderless))

  :config
  (load-theme 'modus-vivendi t))


(use-package osx-trash
  :preface
  (eval-when-compile
    (declare-function osx-trash-setup nil))

  :config
  (osx-trash-setup))


(use-package pbcopy
  :preface
  (eval-when-compile
    (declare-function turn-on-pbcopy nil))

  :config
  (turn-on-pbcopy))


(use-feature emacs
  :demand t

  :preface
  (eval-when-compile
    (declare-function scroll-bar-mode nil)
    (declare-function mac-auto-operator-composition-mode nil)
    (defvar mac-command-modifier)
    (defvar mac-command-key-is-meta)
    (defvar mac-option-modifier)
    (defvar mac-option-key-is-meta))

  :config
  (defvar gemacs-font "PragmataPro Mono")
  (defvar gemacs-font-size 11)

  (setq-default frame-title-format '("%f"))
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  ;; Setup a minimalist frame without toolbar and menu bar, but also taking
  ;; OS behavior quirks into consideration. This is done via `make-frame-func'
  ;; to allow GUI `emacsclient' connecting to `emacs-server' to have different
  ;; frame settings independent of CLI ones.

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

      ;; Mouse goodies

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

  ;; Reset themes before enabling a new one

  (defun gemacs--disable-theme ()
      "Disable theme propogation."
      (mapc #'disable-theme custom-enabled-themes))

  (advice-add 'theme-dont-propagate :before #'gemacs--disable-theme)

  ;; Enable macOS-specific setups

  (gemacs-when-compiletime (eq system-type 'darwin)
    (setq dired-use-ls-dired nil)

    (when (display-graphic-p)
      (defun mac-toggle-fullscreen ()
        (interactive)
        (when (eq window-system 'mac)
          (set-frame-parameter
           nil 'fullscreen
           (when (not (frame-parameter nil 'fullscreen)) 'fullscreen))))

      (setq mac-command-key-is-meta nil)
      (setq mac-command-modifier 'super)
      (setq mac-option-key-is-meta t)
      (setq mac-option-modifier 'meta)

      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-c") 'evil-yank)
      (global-set-key (kbd "s-w") 'delete-window)
      (global-set-key (kbd "s-W") 'delete-frame)
      (global-set-key (kbd "s-n") 'make-frame))

    (use-feature osx-trash :demand t)
    (use-feature pbcopy :demand t))

  ;; Enable theme as late as is humanly possible. This reduces
  ;; frame flashing and other artifacts during startup.

  (add-hook 'gemacs-after-init-hook
    `(lambda ()
       (use-feature telephone-line :demand t)
       (use-feature modus-themes :demand t))))
