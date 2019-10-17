(setq-default frame-title-format '("%f"))
(line-number-mode 1)
(column-number-mode 1)


(eval-when-compile
  (declare-function scroll-bar-mode nil)
  (declare-function mac-auto-operator-composition-mode nil))


(tool-bar-mode -1)
(menu-bar-mode -1)


(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))


(defun gr/make-frame-func (frame)
  "Setup frame attributes after a FRAME is created."
  (when (display-graphic-p frame)
    (let ((w (window-system frame)))
      (cond
        ((eq w 'x)
          (scroll-bar-mode -1)
          (set-frame-font "PragmataPro Mono 11" nil t))
        ((eq w 'mac)
          (progn
            ;; macOS will "float" Emacs window if menu-bar-mode is disabled.
            ;; (e.g. not sticky to Spaces and no fullscreen support)
            (menu-bar-mode 1)
            (scroll-bar-mode -1)
            (set-frame-font "PragmataPro Mono 13" nil t)
            (when (boundp 'mac-auto-operator-composition-mode)
              (mac-auto-operator-composition-mode))))))))

(add-hook 'after-make-frame-functions 'gr/make-frame-func)
(add-hook 'after-init-hook `(lambda () (gr/make-frame-func (selected-frame))))


(use-package ace-window
  :straight t

  :preface
  (eval-when-compile
    (defvar aw-dispatch-always))

  :init
  (setq aw-dispatch-always t)

  (define-key global-map (kbd "M-o") 'ace-window)
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "ww" 'ace-window)))


(use-package kaolin-themes
  :straight t

  :config
  (load-theme 'kaolin-valley-dark t))


(use-package fill-column-indicator
  :diminish fci-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function fci-mode nil))

  :config
  (setq-default fill-column 80)
  (add-hook 'prog-mode-hook 'fci-mode))


(use-package git-gutter
  :diminish git-gutter-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-git-gutter-mode nil))

  :config
  (global-git-gutter-mode t))


(use-package minions
  :after doom-modeline
  :straight t

  :preface
  (eval-when-compile
    (declare-function minions-mode nil))

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "bm" 'minions-minor-modes-menu))

  :config
  (minions-mode t))


(use-package mood-line
  :straight t

  :preface
  (eval-when-compile
    (declare-function mood-line-mode nil))

  :config
  (mood-line-mode t))
