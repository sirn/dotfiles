(req-package evil
  :config
  (evil-mode t))

(req-package evil-anzu
  :diminish (anzu-mode . "")
  :require evil
  :config
  (progn
    (setq anzu-cons-mode-line-p nil)
    (global-anzu-mode t)
    ))

(req-package evil-commentary
  :diminish ""
  :require evil
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

(req-package evil-magit
  :require (evil magit))

(req-package evil-surround
  :config
  (global-evil-surround-mode 1))
