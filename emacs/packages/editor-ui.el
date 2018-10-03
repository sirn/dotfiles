;; macOS will "float" Emacs window if menu-bar-mode is disabled.
;; (e.g. not sticky to Spaces and no fullscreen support)
(when (not (eq window-system 'mac))
  (menu-bar-mode -1))


(setq-default frame-title-format '("%f"))


(eval-when-compile
  (declare-function scroll-bar-mode nil)
  (declare-function mac-auto-operator-composition-mode nil))


(when (display-graphic-p)
  (set-frame-font "PragmataPro Mono 14" nil t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (when (boundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)))


(use-package git-gutter
  :diminish git-gutter-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-git-gutter-mode nil))

  :config
  (global-git-gutter-mode t))


(use-package telephone-line
  :after git-gutter ;; git-gutter is weird
  :straight t

  :preface
  (eval-when-compile
    (defvar projectile-segment)
    (defvar telephone-line-lhs)
    (defvar telephone-line-nil)
    (defvar telephone-line-rhs)
    (defvar telephone-line-primary-left-separator)
    (defvar telephone-line-primary-right-separator)
    (defvar telephone-line-secondary-left-separator)
    (defvar telephone-line-secondary-right-separator)
    (declare-function telephone-line-defsegment* nil)
    (declare-function telephone-line-mode nil))

  :config
  (require 'telephone-line-config)

  (telephone-line-defsegment* projectile-segment ()
    (when (and (boundp 'projectile-project-name))
      (let ((proj (projectile-project-name)))
        (when (not (equal proj "-"))
          proj))))

  (unless (display-graphic-p)
    (setq telephone-line-primary-left-separator 'telephone-line-nil)
    (setq telephone-line-primary-right-separator 'telephone-line-nil)
    (setq telephone-line-secondary-left-separator 'telephone-line-nil)
    (setq telephone-line-secondary-right-separator 'telephone-line-nil))

  (setq telephone-line-lhs
        '((nil    . (telephone-line-window-number-segment))
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
  (setq winum-scope 'visible)
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
