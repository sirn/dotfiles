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

  :defer 1

  :preface
  (eval-when-compile
    (declare-function evil-collection-init nil))

  :config
  (evil-collection-init))


(use-package evil-commentary
  :after evil

  :defer 1

  :preface
  (eval-when-compile
    (declare-function evil-commentary-mode nil))

  :config
  (evil-commentary-mode +1))


(use-package evil-mc
  :after evil

  :defer 1

  :preface
  (eval-when-compile
    (declare-function global-evil-mc-mode nil))

  :config
  (global-evil-mc-mode +1))


(use-package evil-matchit
  :after evil

  :defer 1

  :preface
  (eval-when-compile
    (declare-function global-evil-matchit-mode nil))

  :config
  (global-evil-matchit-mode +1))


(use-package evil-surround
  :after evil

  :defer 1

  :preface
  (eval-when-compile
    (declare-function global-evil-surround-mode nil))

  :config
  (global-evil-surround-mode +1))
