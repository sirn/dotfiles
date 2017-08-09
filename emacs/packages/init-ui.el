(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(when (display-graphic-p)
  (if (eq system-type 'darwin)
    (set-frame-font "Source Code Pro 14" nil t)
    (set-frame-font "Source Code Pro 11" nil t))
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

(when (boundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(req-package minimal-theme)

(req-package telephone-line
  :config
  (progn
    (require 'telephone-line-config)
    (setq telephone-line-primary-right-separator 'telephone-line-abs-left)
    (setq telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
    (setq telephone-line-height (if (eq system-type 'darwin) 22 32))
    (telephone-line-defsegment* custom-winum-segment
      (winum-get-number-string))
    (telephone-line-defsegment* custom-anzu-segment
      (anzu--update-mode-line))
    (setq telephone-line-lhs
          '((nil    . (custom-winum-segment))
            (evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment))
            (nil    . (telephone-line-minor-mode-segment
                       telephone-line-buffer-segment))))
    (setq telephone-line-rhs
          '((nil    . (custom-anzu-segment
                       telephone-line-misc-info-segment))
            (accent . (telephone-line-major-mode-segment))
            (evil   . (telephone-line-airline-position-segment))))
    (telephone-line-mode t)))

(req-package winum
  :require (evil-leader)
  :config
  (setq winum-auto-setup-mode-line nil)
  (setq winum-assign-func 'my-winum-assign-func)
  (winum-mode)
  (evil-leader/set-key
    "0" 'winum-select-window-0
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9))

(defun my-winum-assign-func ()
  (cond
   ((string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 0)
   (t nil)))
