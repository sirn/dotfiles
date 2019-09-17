(use-package markdown-mode
  :straight t

  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :init
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'flycheck-mode))
