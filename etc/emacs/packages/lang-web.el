;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package web-mode
  :mode
  ("\\.[agj]sp\\'"
    "\\.as[cp]x\\'"
    "\\.cjs\\'"
    "\\.djhtml\\'"
    "\\.dtl\\'"
    "\\.erb\\'"
    "\\.html?\\'"
    "\\.jinja2?\\'"
    "\\.mako\\'"
    "\\.mustache\\'"
    "\\.phtml\\'"
    "\\.twig\\'"
    "\\.tpl\\'")

  :custom
  (web-mode-auto-close-style 1)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)

  ;; Indention
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-sql-indent-offset 2)

  ;; Inline script/style
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)

  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil)
    (declare-function flymake-mode nil)
    (defvar web-mode-fontification-off))

  :hook
  ((web-mode . apheleia-mode)
   (web-mode . eglot-ensure)
   (web-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio"))))

  :config
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.js[x]?\\'")))


(use-package css-mode)


(use-package zencoding-mode
  :custom
  (zencoding-preview-default nil)

  :hook
  ((web-mode . zencoding-mode)
   (css-mode . zencoding-mode)))
