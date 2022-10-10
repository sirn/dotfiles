;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package evil
  :demand t

  :custom
  (evil-want-integration +1)
  (evil-want-keybinding nil)
  (evil-mode-line-format nil)
  (evil-undo-system 'undo-tree)

  :preface
  (eval-when-compile
    (defvar evil-want-integration)
    (defvar evil-want-keybinding)
    (defvar evil-mode-line-format)
    (defvar evil-undo-system))

  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-mode +1))


(use-package evil-collection
  :demand t

  :preface
  (eval-when-compile
    (declare-function evil-collection-init nil))

  :config
  (evil-collection-init))


(use-package evil-commentary
  :demand t

  :config
  (evil-commentary-mode +1))


(use-package evil-matchit
  :demand t

  :config
  (global-evil-matchit-mode +1))


(use-package evil-surround
  :demand t

  :config
  (global-evil-surround-mode +1))
