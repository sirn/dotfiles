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
