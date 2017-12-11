(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(req-package avy
  :require evil-leader
  :commands (avy-goto-char avy-goto-char-2 avy-goto-line)
  :init
  (evil-leader/set-key
    "jj" 'avy-goto-char
    "jJ" 'avy-goto-char-2
    "jl" 'avy-goto-line))

(req-package company
  :diminish company-mode
  :init
  (progn
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 2)
    (setq company-require-match nil)
    (setq company-dabbrev-downcase nil)
    (setq company-selection-wrap-around t)
    (setq company-tooltip-flip-when-above t)
    (setq company-tooltip-align-annotations t))
  :config
  (progn
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map [tab] 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous-or-abort)
    (define-key company-active-map [S-tab] 'company-select-previous-or-abort)
    (global-company-mode t)))

(req-package diminish)

(req-package eldoc
  :diminish eldoc-mode)

(req-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(req-package linum
  :require evil-leader
  :commands global-linum-mode
  :init
  (progn
    (setq linum-format "%4d ")
    (evil-leader/set-key
      "tl" 'global-linum-mode)))

(req-package linum-relative
  :require (linum evil-leader)
  :config
  (progn
    (setq linum-relative-current-symbol "")
    (setq linum-relative-format "%4s ")
    (linum-relative-mode)
    (evil-leader/set-key
      "tL" 'linum-relative-toggle)))

(req-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package rainbow-mode
  :commands rainbow-mode
  :diminish rainbow-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(req-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))

(req-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo")))
    (unless (file-exists-p "~/.emacs.d/undo")
      (make-directory "~/.emacs.d/undo")))
  :config
  (progn
    (global-undo-tree-mode)))
