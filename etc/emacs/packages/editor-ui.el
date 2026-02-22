;; -*- lexical-binding: t; no-native-compile: t -*-

;; Select windows by number
(use-package ace-window
  :general
  ("C-x o" #'ace-window)
  (leader
   "ww" #'ace-window)

  :custom
  (aw-scope 'frame)
  (aw-dispatch-always t))


;; Display available keybindings in popup
(use-package which-key
  :hook (after-init . which-key-mode))


;; Builtin: browser-like tabs for window configurations
(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil))


;; Fancy modeline from Doom Emacs
(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon nil)
  (doom-modeline-modal-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version nil)
  (doom-modeline-project-detection 'project)

  :config
  (doom-modeline-mode +1))


;; Icon fonts for various UI elements
(use-package nerd-icons)


;; Accessible and customizable color themes
(use-package modus-themes
  :custom
  (modus-themes-common-palette-overrides
    '((border-mode-line-active bg-mode-line-active)
      (border-mode-line-inactive bg-mode-line-inactive)))

  :config
  (load-theme 'modus-vivendi t))


;; macOS clipboard integration
(use-package pbcopy
  :when (eq system-type 'darwin)

  :preface
  (eval-when-compile
    (declare-function turn-on-pbcopy nil))

  :config
  (turn-on-pbcopy))


;; OSC 52 clipboard for terminal (works over SSH)
(use-package clipetty
  :unless (display-graphic-p)
  :hook (after-init . global-clipetty-mode))


;; Builtin: frame, font, and GUI configuration
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
  (defvar gemacs-font-size 16)
  (defvar gemacs-font-size-ns 14)

  (setq-default frame-title-format '("%f"))
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 120))

  ;; Setup a minimalist frame without toolbar and menu bar, but also taking
  ;; OS behavior quirks into consideration. This is done via `make-frame-func'
  ;; to allow GUI `emacsclient' connecting to `emacs-server' to have different
  ;; frame settings independent of CLI ones.
  (defun gemacs--after-make-frame-func (frame)
    "Setup frame attributes after a FRAME is created."
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (if (display-graphic-p frame)
      (let ((w (window-system frame)))
        (scroll-bar-mode -1)
        (if (eq w 'ns)
            (progn
              ;; macOS will "float" Emacs window if menu-bar-mode is disabled.
              ;; (e.g. not sticky to Spaces and no fullscreen support)
              (menu-bar-mode 1)
              (set-frame-font
               (format
                "-*-%s-regular-*-*-*-%s-*-*-*-*-0-iso10646-1"
                gemacs-font gemacs-font-size-ns)
               nil (list frame)))
          (progn
            (set-frame-font
             (format
              "-*-%s-regular-*-*-*-%s-*-*-*-*-0-iso10646-1"
              gemacs-font gemacs-font-size)
             nil (list frame)))))

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

        (defun gemacs--tty-terminal-has-other-frames (terminal frame)
          "Return t when TERMINAL has frames other than FRAME."
          (let ((found nil))
            (dolist (f (frame-list))
              (when (and (not (eq f frame))
                         (eq (frame-terminal f) terminal))
                (setq found t)))
            found))

        (defun gemacs--maybe-disable-tty-mouse (frame)
          "Disable terminal mouse mode when the last tty FRAME closes.
This avoids leaving xterm mouse reporting enabled after emacsclient exits."
          (when (and (frame-live-p frame)
                     (not (display-graphic-p frame)))
            (let ((terminal (frame-terminal frame)))
              (when (not (gemacs--tty-terminal-has-other-frames terminal frame))
                (xterm-mouse-mode -1)
                (send-string-to-terminal
                 "\e[?1000l\e[?1002l\e[?1003l\e[?1006l"
                 terminal)))))

        (xterm-mouse-mode t)
        (add-hook 'delete-frame-functions #'gemacs--maybe-disable-tty-mouse)
        (bind-key "<wheel-down>" #'gemacs-scroll-down)
        (bind-key "<wheel-up>" #'gemacs-scroll-up)
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
       (require 'doom-modeline)
       (require 'modus-themes))))
