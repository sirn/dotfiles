(req-package evil
  :config
  (progn
    (fset 'evil-visual-update-x-selection 'ignore)
    (evil-mode t)))

(req-package evil-anzu
  :require evil
  :diminish anzu-mode
  :config
  (progn
    (setq anzu-cons-mode-line-p nil)
    (global-anzu-mode t)))

(req-package evil-commentary
  :require evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

(req-package evil-leader
  :require evil
  :config
  (progn
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
      "bD" 'kill-buffer-and-window)))

(req-package evil-matchit
  :require evil
  :config
  (global-evil-matchit-mode 1))

(req-package evil-surround
  :require evil
  :config
  (global-evil-surround-mode 1))
