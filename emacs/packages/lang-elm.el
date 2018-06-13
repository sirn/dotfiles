(use-package elm-mode
  :diminish elm-indent-mode
  :ensure t
  :interpreter "elm"
  :mode "\\.elm\\'"

  :init
  (setq elm-sort-imports-on-save t)
  (setq elm-format-on-save t)

  (eval-after-load 'company
    (progn
      (defun setup-company-elm ()
        (set (make-local-variable 'company-backends) '(company-elm)))
      (add-hook 'elm-mode-hook 'setup-company-elm))))


(use-package flycheck-elm
  :after (flycheck elm-mode)
  :commands flycheck-elm-setup
  :ensure t

  :init
  (add-hook 'elm-mode-hook 'flycheck-elm-setup))
