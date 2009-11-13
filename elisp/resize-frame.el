;; Resize Frame on Horizontal Splitting.
;; Written by Kridsada Thanabulpong <naises@gmail.com>.
;; Published as public domain.

(setq default-width (frame-width))
(when window-system
  (add-hook 'window-configuration-change-hook
            '(lambda ()
               (setq windows ())
               (dolist (w (window-list))
                 (setq windows (cons (car (window-edges w)) windows)))
               (set-frame-width (selected-frame)
                                (* default-width
                                   (length (delete-dups windows)))))))