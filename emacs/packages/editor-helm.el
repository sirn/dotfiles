(use-package helm
  :defer 1
  :demand t
  :diminish helm-mode
  :ensure t

  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)

   :map helm-map
   ("C-j"     . helm-next-line)
   ("C-k"     . helm-previous-line)
   ("C-h"     . helm-next-source)
   ("C-S-h"   . describe-key))

  :config
  (require 'helm-config)
  (setq helm-split-window-inside-p t)
  (setq helm-echo-input-in-header-line t)
  (helm-mode 1)

  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "bb" 'helm-buffers-list))

  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                    (let ((bg-color (face-background 'default nil)))
                      `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level))))


(use-package helm-flycheck
  :after (flycheck helm)
  :ensure t

  :bind
  (:map flycheck-mode-map
   ("C-c ! h" . helm-flycheck)))


(use-package helm-projectile
  :after (helm projectile)
  :ensure t

  :preface
  (eval-when-compile
    (declare-function helm-projectile-on nil))

  :commands
  (helm-projectile-rg
   helm-projectile-find-file
   helm-projectile-switch-project
   helm-projectile-switch-to-buffer)

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "p/" 'helm-projectile-rg
      "pf" 'helm-projectile-find-file
      "pp" 'helm-projectile-switch-project
      "pb" 'helm-projectile-switch-to-buffer))

  :config
  (helm-projectile-on))


(use-package helm-rg
  :after helm
  :commands helm-rg
  :ensure t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "/" 'helm-rg)))
