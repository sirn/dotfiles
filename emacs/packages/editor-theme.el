(use-package doom-themes
  :straight (doom-themes :files (:defaults "themes/*.el")
                         :type git
                         :host github
                         :repo "hlissner/emacs-doom-themes"
                         :fork (:host github
                                      :repo "sirn/emacs-doom-themes"
                                      :branch "256col-fixes"))

  :config
  (load-theme 'doom-city-lights t)
  (when (<= (tty-display-color-cells) 256)
    (custom-theme-set-faces
     'doom-city-lights
     '(magit-diff-added             ((t :foreground "#bacea1" :background nil)))
     '(magit-diff-added-highlight   ((t :foreground "#99bb66" :background nil)))
     '(magit-diff-removed           ((t :foreground "#f4a98b" :background nil)))
     '(magit-diff-removed-highlight ((t :foreground "#ff6655" :background nil))))))


(use-package solaire-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function turn-on-solaire-mode nil)
    (declare-function solaire-mode-in-minibuffer nil)
    (declare-function solaire-mode-swap-bg nil))

  :config
  (add-hook 'after-revert-hook 'turn-on-solaire-mode)
  (add-hook 'change-major-mode-hook 'turn-on-solaire-mode)
  (add-hook 'ediff-prepare-buffer-hook 'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook 'solaire-mode-in-minibuffer))
