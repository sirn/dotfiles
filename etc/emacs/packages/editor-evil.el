;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package evil
  :demand t

  :custom
  (evil-want-integration +1)
  (evil-want-keybinding nil)
  (evil-mode-line-format nil)
  (evil-undo-system 'undo-tree)

  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-mode +1))


(use-package evil-collection
  :after evil
  :demand t

  :config
  (evil-collection-init))


(use-package evil-commentary
  :after evil
  :demand t

  :config
  (evil-commentary-mode +1))


(use-package evil-mc
  :after evil
  :demand t

  :general
  (:states 'visual
   :keymaps 'evil-mc-key-map
   "A" #'evil-mc-make-cursor-in-visual-selection-end
   "I" #'evil-mc-make-cursor-in-visual-selection-beg)

  :config
  (global-evil-mc-mode +1))


(use-package evil-matchit
  :after evil
  :demand t

  :config
  (global-evil-matchit-mode +1))


(use-package evil-surround
  :after evil
  :demand t

  :config
  (global-evil-surround-mode +1))
