(use-package eshell
  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "'" 'eshell)))


(use-package multi-term
  :commands multi-term
  :straight t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "\"" 'multi-term)))
