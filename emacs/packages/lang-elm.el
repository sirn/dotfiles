(defun custom/setup-company-elm ()
  (set (make-local-variable 'company-backends) '(company-elm)))

(req-package elm-mode
  :mode "\\.elm\\'"
  :interpreter "elm"
  :init
  (add-hook 'elm-mode-hook 'custom/setup-company-elm)
  :config
  (progn
    (setq elm-sort-imports-on-save t)
    (setq elm-format-on-save t)))

(req-package flycheck-elm
  :require (flycheck elm-mode)
  :commands flycheck-elm-setup
  :init
  (add-hook 'elm-mode-hook #'flycheck-elm-setup))
