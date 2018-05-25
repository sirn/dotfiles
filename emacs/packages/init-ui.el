(defun custom/winum-assign-func ()
  (when (and (boundp 'neo-buffer-name)
             (string= (buffer-name) neo-buffer-name)
             (eq (selected-window) (frame-first-window)))
    0))

(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(when (display-graphic-p)
  (set-frame-font "PragmataPro 14" nil t)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

(when (boundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(req-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))

(req-package tao-theme
  :config
  (load-theme 'tao-yin t))

(req-package telephone-line
  :config
  (progn
    (require 'telephone-line-config)
    (telephone-line-defsegment* custom/winum-segment ()
      (winum-get-number-string))
    (telephone-line-defsegment* custom/anzu-segment ()
      (anzu--update-mode-line))
    (setq telephone-line-lhs
          '((nil    . (custom/winum-segment))
            (evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment))
            (nil    . (telephone-line-minor-mode-segment
                       telephone-line-buffer-segment))))
    (setq telephone-line-rhs
          '((nil    . (custom/anzu-segment
                       telephone-line-misc-info-segment))
            (accent . (telephone-line-major-mode-segment))
            (evil   . (telephone-line-airline-position-segment))))
    (telephone-line-mode t)))

(req-package winum
  :require evil-leader
  :config
  (progn
    (setq winum-auto-setup-mode-line nil)
    (add-to-list 'winum-assign-functions 'custom/winum-assign-func)
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
      "9" 'winum-select-window-9)))
