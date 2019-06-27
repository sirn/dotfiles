(use-package minions
  :after doom-modeline
  :straight t

  :preface
  (declare-function minions-mode nil)

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "bm" 'minions-minor-modes-menu))

  :config
  (minions-mode t))


(use-package doom-modeline
  :straight t

  :preface
  (declare-function doom-modeline-mode nil)

  :config
  (doom-modeline-mode t))
