(use-package omnifocus-capture
  :commands send-region-to-omnifocus
  :straight (:repo "https://gist.github.com/sirn/014f674702b9c2d2fa92feac55f06854")

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "ss" 'send-region-to-omnifocus)))
