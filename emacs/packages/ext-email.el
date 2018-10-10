(use-package notmuch
  :commands notmuch
  :straight t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "mm" 'notmuch
      "m/" 'counsel-notmuch)))

(use-package counsel-notmuch
  :after (counsel notmuch)
  :commands counsel-notmuch
  :straight t)
