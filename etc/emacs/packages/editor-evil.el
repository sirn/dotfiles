;; -*- lexical-binding: t; no-native-compile: t -*-

;; Vim emulation layer
(use-package evil
  :demand t

  :custom
  (evil-want-integration +1)
  (evil-want-keybinding nil)
  (evil-mode-line-format nil)
  (evil-undo-system 'undo-redo)

  :config
  (defalias 'evil-visual-update-x-selection #'ignore)
  (evil-mode +1))


;; Evil keybindings for many modes
(use-package evil-collection
  :after evil

  :preface
  (eval-when-compile
    (declare-function evil-collection-init nil))

  :config
  (evil-collection-init))


;; Comment/uncomment with gc
(use-package evil-commentary
  :after evil

  :preface
  (eval-when-compile
    (declare-function evil-commentary-mode nil))

  :config
  (evil-commentary-mode +1))


;; Multiple cursors for evil
(use-package evil-mc
  :after evil

  :preface
  (eval-when-compile
    (declare-function global-evil-mc-mode nil))

  :config
  (global-evil-mc-mode +1))


;; Jump between matching tags with %
(use-package evil-matchit
  :after evil

  :preface
  (eval-when-compile
    (declare-function global-evil-matchit-mode nil))

  :config
  (global-evil-matchit-mode +1))


;; Surround text with quotes, brackets, etc.
(use-package evil-surround
  :after evil

  :preface
  (eval-when-compile
    (declare-function global-evil-surround-mode nil))

  :config
  (global-evil-surround-mode +1))


;; Evil keybindings for org-mode
(use-package evil-org
  :after evil

  :preface
  (eval-when-compile
    (declare-function evil-org-set-key-theme nil)
    (declare-function evil-org-agenda-set-keys nil))

  :hook
  (org-mode . evil-org-mode)

  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
