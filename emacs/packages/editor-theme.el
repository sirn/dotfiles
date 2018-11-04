(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))


(use-package doom-themes
  :straight t

  :config
  (load-theme 'doom-city-lights t))


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
