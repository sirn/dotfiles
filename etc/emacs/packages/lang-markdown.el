;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package pandoc-mode
  :init
  (add-hook 'rst-mode-hook #'pandoc-mode)
  (add-hook 'markdown-mode-hook #'pandoc-mode)
  (add-hook 'pandoc-mode-hook #'apheleia-mode)

  :config
  (use-feature apheleia
    :demand t

    :config
    (add-to-list 'apheleia-formatters '(pandoc-gfm . ("pandoc" "-t" "gfm" "--wrap" "preserve")))
    (add-to-list 'apheleia-formatters '(pandoc-markdown . ("pandoc" "-t" "markdown" "--wrap" "preserve")))
    (add-to-list 'apheleia-formatters '(pandoc-rst . ("pandoc" "-t" "rst" "--wrap" "preserve")))
    (add-to-list 'apheleia-mode-alist '(gfm-mode . pandoc-gfm))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . pandoc-markdown))
    (add-to-list 'apheleia-mode-alist '(rst-mode . pandoc-rst))))


(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :custom
  (markdown-command "pandoc"))
