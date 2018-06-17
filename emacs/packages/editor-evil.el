(use-package evil
  :straight t
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-mode t))


(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function evil-commentary-mode nil))

  :config
  (with-eval-after-load 'evil-commentary
    (evil-commentary-mode t)))


(use-package evil-leader
  :after evil
  :straight t

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
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-evil-matchit-mode nil))

  :config
  (global-evil-matchit-mode t))


(use-package evil-surround
  :after evil
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-evil-surround-mode nil))

  :config
  (global-evil-surround-mode t))
