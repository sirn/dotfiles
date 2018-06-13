(use-package evil
  :ensure t
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-mode t))


(use-package evil-anzu
  :after evil
  :diminish anzu-mode
  :ensure t

  :config
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode t))


(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function evil-commentary-mode nil))

  :config
  (eval-after-load 'evil-commentary
    (evil-commentary-mode t)))


(use-package evil-leader
  :after evil
  :ensure t

  :preface
  (eval-when-compile
    (declare-function global-evil-leader-mode nil)
    (declare-function evil-leader/set-leader nil)
    (declare-function evil-leader/set-key nil))

  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode 1)
  (evil-leader/set-key
    "wo" 'other-window
    "wd" 'delete-window
    "wD" 'delete-other-windows
    "w-" 'split-window-below
    "w/" 'split-window-right
    "w=" 'balance-windows
    "bd" 'kill-buffer
    "bD" 'kill-buffer-and-window))


(use-package evil-matchit
  :after evil
  :ensure t

  :preface
  (eval-when-compile
    (declare-function global-evil-matchit-mode nil))

  :config
  (global-evil-matchit-mode t))


(use-package evil-surround
  :after evil
  :ensure t

  :preface
  (eval-when-compile
    (declare-function global-evil-surround-mode nil))

  :config
  (global-evil-surround-mode t))
