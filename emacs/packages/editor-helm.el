(defun custom/helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(req-package helm
  :require evil-leader
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (setq helm-split-window-in-side-p t)
    (setq helm-echo-input-in-header-line t)
    (helm-mode 1)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (evil-leader/set-key
      "bb" 'helm-buffers-list)
    (add-hook 'helm-minibuffer-set-up-hook 'custom/helm-hide-minibuffer-maybe)
    (with-eval-after-load 'helm-files
      (dolist (keymap (list helm-find-files-map helm-read-file-map))
        (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
        (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)))))

(req-package helm-flycheck
  :require (flycheck helm)
  :commands helm-flycheck
  :init
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(req-package helm-projectile
  :require (evil-leader helm projectile)
  :commands (helm-projectile-rg
             helm-projectile-find-file
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (evil-leader/set-key
    "p/" 'helm-projectile-rg
    "pf" 'helm-projectile-find-file
    "pp" 'helm-projectile-switch-project
    "pb" 'helm-projectile-switch-to-buffer)
  :config
  (helm-projectile-on))

(req-package helm-rg
  :require (evil-leader helm)
  :commands helm-rg
  :init
  (progn
    (evil-leader/set-key
      "/" 'helm-rg)))
