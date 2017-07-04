(req-package helm
  :require evil-leader
  :diminish (helm-mode . "")
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files))
  :init
  (evil-leader/set-key
    "bb" 'helm-buffers-list)
  :config
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (with-eval-after-load 'helm-files
      (dolist (keymap (list helm-find-files-map helm-read-file-map))
        (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
        (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)))))

(req-package helm-ag
  :require evil-leader ;; helm load is deferred
  :commands helm-do-ag
  :init
  (custom-set-variables '(helm-ag-base-command "rg --color=never --no-heading"))
  (evil-leader/set-key
    "/" 'helm-do-ag))

(req-package helm-flycheck
  :require flycheck ;; helm load is deferred
  :commands helm-flycheck
  :init
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(req-package helm-projectile
  :require (evil-leader projectile) ;; helm load is deferred
  :commands (helm-projectile-ag helm-projectile-find-file)
  :init
  (evil-leader/set-key
    "p/" 'helm-projectile-ag
    "pf" 'helm-projectile-find-file)
  :config
  (helm-projectile-on)

  ;; Workaround for ripgrep and helm-projectile-ag.
  ;; https://github.com/syohex/emacs-helm-ag/issues/283#issuecomment-261415790
  (defun helm-projectile-ag (&optional options)
    (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
    (if (require 'helm-ag nil  'noerror)
	(if (projectile-project-p)
	    (let ((helm-ag-command-option options)
		  (current-prefix-arg nil))
	      (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
	  (error "You're not in a project"))
      (error "Package helm-ag not available"))))
