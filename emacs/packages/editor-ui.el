;; macOS will "float" Emacs window if menu-bar-mode is disabled.
;; (e.g. not sticky to Spaces and no fullscreen support)
(when (not (eq window-system 'mac))
  (menu-bar-mode -1))


(setq-default frame-title-format '("%f"))


(when (display-graphic-p)
  (set-frame-font "PragmataPro 14" nil t)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (when (boundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)))


(use-package git-gutter
  :defer 2
  :diminish git-gutter-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-git-gutter-mode nil))

  :config
  (global-git-gutter-mode t))


(use-package telephone-line
  :straight t

  :preface
  (eval-when-compile
    (declare-function telephone-line-mode nil))

  :config
  (require 'telephone-line-config)

  (telephone-line-defsegment* winum-segment () (winum-get-number-string))
  (telephone-line-defsegment* projectile-segment ()
    (when (and (boundp 'projectile-project-name))
      (let ((proj (projectile-project-name)))
        (when (not (equal proj "-"))
          proj))))

  (setq telephone-line-lhs
        '((nil    . (winum-segment))
          (evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (projectile-segment
                     telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

  (telephone-line-mode t))


(use-package winum
  :straight t

  :preface
  (eval-when-compile
    (declare-function winum-mode nil))

  :config
  (defun winum-assign-func ()
    (when (and (boundp 'neo-buffer-name)
               (string= (buffer-name) neo-buffer-name)
               (eq (selected-window) (frame-first-window))) 0))
  (add-to-list 'winum-assign-functions 'winum-assign-func)

  (setq winum-auto-setup-mode-line nil)
  (winum-mode)

  (with-eval-after-load 'evil-leader
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
