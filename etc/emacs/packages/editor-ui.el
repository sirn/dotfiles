(setq-default frame-title-format '("%f"))
(line-number-mode 1)
(column-number-mode 1)


(eval-when-compile
  (declare-function scroll-bar-mode nil)
  (declare-function mac-auto-operator-composition-mode nil))


(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


(defun gr/make-frame-func (frame)
  "Setup frame attributes after a FRAME is created."
  (if (display-graphic-p frame)
    (let ((w (window-system frame)))
      (cond
       ((eq w 'x) (set-frame-font "PragmataPro Mono 11" nil t))
       ((eq w 'mac)
        (progn
          ;; macOS will "float" Emacs window if menu-bar-mode is disabled.
          ;; (e.g. not sticky to Spaces and no fullscreen support)
          (menu-bar-mode 1)
          (set-frame-font "PragmataPro Mono 14" nil t)
          (when (boundp 'mac-auto-operator-composition-mode)
            (mac-auto-operator-composition-mode))))))))

(add-hook 'after-make-frame-functions 'gr/make-frame-func)


(use-package git-gutter
  :diminish git-gutter-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-git-gutter-mode nil))

  :config
  (global-git-gutter-mode t))


(use-package winum
  :straight t

  :preface
  (eval-when-compile
    (declare-function winum-mode nil))

  :config
  (defun gr/winum-assign-func ()
    (when (and (boundp 'neo-buffer-name)
               (string= (buffer-name) neo-buffer-name)
               (eq (selected-window) (frame-first-window))) 0))
  (add-to-list 'winum-assign-functions 'gr/winum-assign-func)

  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local)
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
