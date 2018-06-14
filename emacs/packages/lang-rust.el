(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)

  :init
  (setq rust-format-on-save t))


(use-package racer
  :after rust-mod
  :diminish racer-mode
  :ensure t

  :preface
  (declare-function racer-mode nil)

  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (with-eval-after-load 'eldoc
    (add-hook 'racer-mode-hook 'eldoc-mode)))
