(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil)

  (when (display-graphic-p)
    (defun mac-toggle-fullscreen ()
      (interactive)
      (when (eq window-system 'mac)
        (set-frame-parameter
         nil 'fullscreen
         (when (not (frame-parameter nil 'fullscreen)) 'fullscreen))))

    (setq mac-command-key-is-meta nil)
    (setq mac-command-modifier 'super)
    (setq mac-option-key-is-meta t)
    (setq mac-option-modifier 'meta)

    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-c") 'evil-yank)
    (global-set-key (kbd "s-w") 'delete-window)
    (global-set-key (kbd "s-W") 'delete-frame)
    (global-set-key (kbd "s-n") 'make-frame))

  (req-package osx-trash
    :init (osx-trash-setup))

  (req-package pbcopy
    :init (turn-on-pbcopy)))
