(defun custom/elixir-smartparens-hook ()
  (sp-with-modes '(elixir-mode)
    (sp-local-pair
     "fn" "end"
     :when '(("SPC" "RET"))
     :actions '(insert navigate))
    (sp-local-pair
     "do" "end"
     :when '(("SPC" "RET"))
     :actions '(insert navigate))))

(req-package alchemist
  :require elixir-mode
  :diminish alchemist-mode
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(req-package elixir-mode
  :mode ("\\.exs?\\'" "\\.elixir\\'")
  :interpreter "elixir")

(req-package flycheck-mix
  :require (flycheck elixir-mode)
  :commands flycheck-mix-setup
  :init
  (add-hook 'elixir-mode-hook #'flycheck-mix-setup))
