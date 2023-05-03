;; -*- lexical-binding: t; no-native-compile: t -*-

;; This file wraps provides a standalone magit, i.e.
;; emacs -nw -Q -l magit.el

;; --------------------------------------------------------------------------
;;; Early configurations

;; Prevent package.el from modifying this file.

(setq package-enable-at-startup nil)

;; Disable byte-compilation warnings from native-compiled packages
;; from being reported asynchronously into the UI.

(setq native-comp-async-report-warnings-errors nil)

;; Setup base emacs directory for standalone Magit

(setq user-emacs-directory (format "~/.emacs.d/magit/%s/" emacs-version))

;; Use dedicated custom.el file within Magit directory.

(setq custom-file (concat user-emacs-directory "custom.el"))

;; --------------------------------------------------------------------------
;;; GnuTLS

(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls)
    (defvar gnutls-trustfiles))

  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 3072)
  (when (< emacs-major-version 27)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

  (let ((cert "/usr/local/etc/libressl/cert.pem"))
    (when (file-exists-p cert)
      (add-to-list 'gnutls-trustfiles cert))))

;; --------------------------------------------------------------------------
;;; Package configurations

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'el-patch)

(eval-when-compile
  (defvar straight-use-package-by-default)
  (defvar use-package-always-defer)
  (defvar use-package-compute-statistics))

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq use-package-compute-statistics nil)

(eval-when-compile
  (require 'use-package)
  (require 'el-patch))

;; Key bindings are handled by general.el, which replaces both bind-key
;; and evil-leader; this is loaded early to allow use-package macro
;; to work correctly.

(use-package general)

(general-create-definer leader
  :keymaps 'override
  :states '(normal visual motion insert)
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

;; --------------------------------------------------------------------------
;;; Convenient helpers

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; --------------------------------------------------------------------------
;;; Saner defaults

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; --------------------------------------------------------------------------
;;; Magit

(use-package magit
  :general
  (leader
    "gs" #'magit-project-status)

  :custom
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-bind-magit-project-status nil)
  (magit-remote-set-if-missing t)

  :init
  (defun setup-standalone-magit ()
    (magit-status)
    (delete-other-windows)
    (evil-local-set-key 'normal "q" #'save-buffers-kill-emacs))

  (add-hook 'after-init-hook 'setup-standalone-magit))

;; --------------------------------------------------------------------------
;;; Evil

(use-package evil
  :demand t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init 'magit)
  (evil-mode +1))

;; --------------------------------------------------------------------------
;;; Project

(use-package project
  :demand t)

;; --------------------------------------------------------------------------
;;; Selection

(use-package selectrum
  :straight (:host github :repo "radian-software/selectrum")
  :demand t
  :config
  (selectrum-mode +1))


(use-package prescient
  :demand t
  :custom
  (prescient-history-length 1000)
  :config
  (prescient-persist-mode +1)
  (use-feature emacs
    :custom
    (completion-styles '(prescient basic))))


(use-package selectrum-prescient
  :straight (:host github
              :repo "radian-software/prescient.el"
              :files ("selectrum-prescient.el"))

  :after (selectrum prescient)
  :demand t
  :config
  (selectrum-prescient-mode +1))

;; --------------------------------------------------------------------------
;;; UI

(use-feature emacs
  :general
  (leader
    "wo"  #'other-window
    "wd"  #'delete-window
    "wD"  #'delete-other-windows
    "w-"  #'split-window-below
    "w/"  #'split-window-right
    "w="  #'balance-windows
    "bd"  #'kill-buffer
    "bD"  #'kill-buffer-and-window
    "bb"  #'switch-to-buffer
    "wbb" #'switch-to-buffer-other-window)

  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(use-package telephone-line
  :init
  (defun setup-modeline ()
    (telephone-line-mode +1))

  (add-hook 'after-init-hook 'setup-modeline))

(use-package modus-themes
  :demand t
  :init
  (defun setup-ui ()
    (setq modus-themes-mode-line '(borderless))
    (load-theme 'modus-vivendi t))

  (add-hook 'after-init-hook 'setup-ui))

;; --------------------------------------------------------------------------
;;; Finalizing

(run-hooks 'after-init-hook)
