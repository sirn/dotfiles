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
    "\\.phtml\\'")

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

  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (defvar web-mode-fontification-off))

  :init
  (add-hook 'web-mode-hook #'apheleia-mode)

  :config
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.js[x]?\\'")))


(use-package css-mode)


(use-package zencoding-mode
  :custom
  (zencoding-preview-default nil)

  :init
  (add-hook 'web-mode-hook 'zencoding-mode)
  (add-hook 'css-mode-hook 'zencoding-mode))
