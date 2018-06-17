(use-package elm-mode
  :diminish elm-indent-mode
  :interpreter "elm"
  :mode "\\.elm\\'"
  :straight t

  :init
  (setq elm-sort-imports-on-save t)
  (setq elm-format-on-save t)

  (with-eval-after-load 'company
    (defun setup-company-elm ()
      (set (make-local-variable 'company-backends) '(company-elm)))
    (add-hook 'elm-mode-hook 'setup-company-elm)))


(use-package flycheck-elm
  :after elm-mode
  :commands flycheck-elm-setup
  :straight t

  :init
  (with-eval-after-load 'flycheck
    (add-hook 'elm-mode-hook 'flycheck-elm-setup)))
