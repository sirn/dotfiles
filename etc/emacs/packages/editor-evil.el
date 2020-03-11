;; -*- lexical-binding: t -*-

(use-package evil
  :demand t

  :init
  (setq evil-want-integration +1)
  (setq evil-want-keybinding nil)
  (setq evil-mode-line-format nil)

  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-mode +1))


(use-package evil-collection
  :after evil
  :demand t

  :preface
  (eval-when-compile
    (declare-function evil-collection-init nil))

  :config
  (evil-collection-init))


(use-package evil-commentary
  :after evil
  :demand t

  :config
  (evil-commentary-mode +1))


(use-package evil-leader
  :after evil
  :demand t

  :leader
  ("wo" #'other-window
   "wd" #'delete-window
   "wD" #'delete-other-windows
   "w-" #'split-window-below
   "w/" #'split-window-right
   "w=" #'balance-windows
   "bd" #'kill-buffer
   "bD" #'kill-buffer-and-window)

  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode +1))


(use-package evil-matchit
  :after evil
  :demand t

  :config
  (global-evil-matchit-mode +1))


(use-package evil-magit
  :after (evil magit)
  :demand t)


(use-package evil-surround
  :after evil
  :demand t

  :config
  (global-evil-surround-mode +1))
