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
  (enable-circe-new-day-notifier))
