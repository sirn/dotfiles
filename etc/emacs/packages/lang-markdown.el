;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package pandoc-mode
  :init
  (add-hook 'rst-mode-hook #'pandoc-mode)
  (add-hook 'markdown-mode-hook #'pandoc-mode))


(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :custom
  (markdown-command "pandoc")

  :init
  (add-hook 'markdown-mode-hook #'apheleia-mode)

  :config
  (use-feature apheleia
    :demand t

    :config
    (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))))
