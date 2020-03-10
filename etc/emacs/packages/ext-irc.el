(use-package circe
  :commands circe
  :straight t

  :preface
  (eval-when-compile
    (defvar circe-network-defaults)
    (defvar circe-server-buffer-name))

  :init
  (setq circe-network-defaults '())
  (setq circe-server-buffer-name "*irc-{network}*")

  (defun gr/ivy-switch-irc-buffers ()
    (interactive)
    (ivy-read "Switch to channel: "
              (seq-filter
               '(lambda (name)
                  (or (string-prefix-p "#" name)
                      (string-prefix-p "*irc-" name)))
               (map 'list 'buffer-name (buffer-list)))
              :action 'switch-to-buffer))

  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "ib" 'gr/ivy-switch-irc-buffers))

  :config
  (require 'circe-color-nicks)
  (require 'circe-new-day-notifier)
  (enable-circe-color-nicks)
  (enable-circe-new-day-notifier)

  (setq lui-time-stamp-format "%H:%M")
  (setq lui-time-stamp-position 'right-margin)
  (setq lui-fill-type nil)

  (defun gr/lui-setup ()
    (setq fringes-outside-margins t)
    (setq right-margin-width 5)
    (setq word-wrap t)
    (setq wrap-prefix "    ")
    (if (display-graphic-p)
      (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)))

  (add-hook 'lui-mode-hook 'gr/lui-setup))
