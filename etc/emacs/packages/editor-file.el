;; -*- lexical-binding: t; no-native-compile: t -*-

;; Builtin: directory editor
(use-package dired
  :general
  ("C-x C-j" #'dired-jump)

  :config
  (pcase system-type
    ((or 'darwin 'berkeley-unix)
     (setq dired-use-ls-dired nil))))


;; Nerd icons in dired buffers
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; File tree sidebar
(use-package treemacs
  :general
  (leader
    "t t" #'treemacs
    "t T" #'treemacs-select-window
    "t p a" #'treemacs-add-project-to-workspace
    "t w c" #'treemacs-create-workspace
    "t w e" #'treemacs-edit-workspaces
    "t w r" #'treemacs-rename-workspace
    "t w w" #'treemacs-switch-workspace)

  :custom
  (treemacs-read-string-input 'from-minibuffer)

  :config
  (require 'treemacs-evil)
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons")
  (treemacs-hide-gitignored-files-mode +1))


;; Nerd icons for treemacs
(use-package treemacs-nerd-icons)


;; Evil keybindings for treemacs
(use-package treemacs-evil)


;; Use macOS Trash instead of rm
(use-package osx-trash
  :when (eq system-type 'darwin)

  :preface
  (eval-when-compile
    (declare-function osx-trash-setup nil))

  :config
  (osx-trash-setup))
