;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package ace-window
  :demand t

  :general
  ("C-x o" #'ace-window)
  (leader
   "ww" #'ace-window)

  :config
  (setq aw-scope 'frame)
  (setq aw-dispatch-always t))


(use-package which-key
  :demand t

  :config
  (which-key-mode +1))


;; Builtin
(use-package tab-bar
  :demand t

  :init
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-close-button-show nil))


(use-package telephone-line
  :custom
  (telephone-line-height 18)
  (telephone-line-primary-right-separator 'telephone-line-utf-abs-right)
  (telephone-line-secondary-right-separator 'telephone-line-utf-abs-hollow-right)
  (telephone-line-primary-left-separator 'telephone-line-utf-abs-left)
  (telephone-line-secondary-left-separator 'telephone-line-utf-abs-hollow-left)
  (telephone-line-lhs
    '((evil   . (telephone-line-evil-tag-segment))
      (nil    . (telephone-line-vc-segment))
      (nil    . (telephone-line-project-segment))
      (nil    . (telephone-line-process-segment
                 telephone-line-buffer-segment))))
  (telephone-line-rhs
    '((nil    . (telephone-line-flycheck-segment))
      (nil    . (telephone-line-misc-info-segment))
      (nil    . (telephone-line-major-mode-segment))
      (evil   . (telephone-line-airline-position-segment))))

  :config
  (telephone-line-mode +1))


(use-package nerd-icons)


(use-package modus-themes
  :custom
  (modus-themes-common-palette-overrides
    '((border-mode-line-active bg-mode-line-active)
      (border-mode-line-inactive bg-mode-line-inactive)))

  :config
  (load-theme 'modus-vivendi t))


(use-package pbcopy
  :when (eq system-type 'darwin)

  :preface
  (eval-when-compile
    (declare-function turn-on-pbcopy nil))

  :config
  (turn-on-pbcopy))


;; Builtin
(use-package emacs
  :preface
  (eval-when-compile
    (declare-function scroll-bar-mode nil)
    (declare-function mac-auto-operator-composition-mode nil)
    (defvar mac-command-modifier)
    (defvar mac-command-key-is-meta)
    (defvar mac-option-modifier)
    (defvar mac-option-key-is-meta))

  :config
  (defvar gemacs-font "PragmataPro Mono Liga")
  (defvar gemacs-font-size 11)

  (setq-default frame-title-format '("%f"))
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 120))

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
          ((or (eq w 'x) (eq w 'pgtk))
           (scroll-bar-mode -1)
           (set-face-attribute 'default nil
               :font (format "%s %s" gemacs-font gemacs-font-size)))
          ((eq w 'mac)
           ;; macOS will "float" Emacs window if menu-bar-mode is disabled.
           ;; (e.g. not sticky to Spaces and no fullscreen support)
           (menu-bar-mode 1)
           (scroll-bar-mode -1)
           ;; macOS display font size about x1.2 smaller than other Unices.
           (set-face-attribute 'default nil
              :font (format "%s %s" gemacs-font (round (* gemacs-font-size 1.2))))
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

    (require 'pbcopy))

  ;; Enable theme as late as is humanly possible. This reduces
  ;; frame flashing and other artifacts during startup.

  (add-hook 'gemacs-after-init-hook
    `(lambda ()
       (require 'telephone-line)
       (require 'modus-themes))))
