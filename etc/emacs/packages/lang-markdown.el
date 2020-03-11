;; -*- lexical-binding: t -*-

(use-package pandoc-mode
  :commands pandoc-mode

  :init
  (add-hook 'rst-mode-hook #'pandoc-mode)
  (add-hook 'markdown-mode-hook #'pandoc-mode))


(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :init
  (setq markdown-command "pandoc"))
