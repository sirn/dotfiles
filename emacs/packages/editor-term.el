(use-package multi-term
  :commands multi-term
  :ensure t

  :init
  (eval-after-load 'evil-leader
    (evil-leader/set-key
      "'" 'multi-term))

  :config
  (eval-after-load 'evil
    (progn
      (evil-set-initial-state 'term-mode 'emacs)
      (evil-define-key 'normal term-raw-map "i" 'evil-emacs-state)
      (evil-define-key 'normal term-raw-map "a" 'evil-emacs-state)

      ;; Make sure emacs key bindings in term (urgh) is handled by shell.
      (evil-define-key 'emacs term-raw-map (kbd "C-a") '(lambda () (interactive) (term-send-raw-string "\C-a")))
      (evil-define-key 'emacs term-raw-map (kbd "C-e") '(lambda () (interactive) (term-send-raw-string "\C-e")))
      (evil-define-key 'emacs term-raw-map (kbd "C-f") '(lambda () (interactive) (term-send-raw-string "\C-f")))
      (evil-define-key 'emacs term-raw-map (kbd "C-k") '(lambda () (interactive) (term-send-raw-string "\C-k")))
      (evil-define-key 'emacs term-raw-map (kbd "C-l") '(lambda () (interactive) (term-send-raw-string "\C-l")))
      (evil-define-key 'emacs term-raw-map (kbd "C-n") '(lambda () (interactive) (term-send-raw-string "\C-n")))
      (evil-define-key 'emacs term-raw-map (kbd "C-p") '(lambda () (interactive) (term-send-raw-string "\C-p")))
      (evil-define-key 'emacs term-raw-map (kbd "C-u") '(lambda () (interactive) (term-send-raw-string "\C-u")))
      (evil-define-key 'emacs term-raw-map (kbd "C-y") '(lambda () (interactive) (term-send-raw-string "\C-y"))))))
