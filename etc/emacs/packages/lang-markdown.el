(use-package markdown-mode
  :straight t

  :init
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'flycheck-mode))


(use-package poly-markdown
  :straight t

  :mode
  (("README\\.md\\'" . poly-gfm-mode)
   ("\\.md\\'" . poly-markdown-mode)
   ("\\.markdown\\'" . poly-markdown-mode)))
