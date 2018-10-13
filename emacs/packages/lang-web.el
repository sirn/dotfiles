(use-package emmet-mode
  :after web-mode
  :commands emmet-mode
  :diminish emmet-mode
  :straight t

  :preface
  (defvar web-mode-enable-current-element-highlight)
  (defvar web-mode-enable-current-column-highlight)

  :init
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


(use-package web-mode
  :straight t
  :mode
  ("\\.phtml\\'"
   "\\.tpl\\.php\\'"
   "\\.[agj]sp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"
   "\\.mako\\'"
   "\\.jinja2?\\'"
   "\\.mustache\\'"
   "\\.djhtml\\'"
   "\\.html?\\'"
   "\\.tsx\\'")

  :preface
  (eval-when-compile
    (declare-function gr/setup-tide nil))

  :init
  (when (boundp 'gr/setup-tide)
    (defun gr/setup-web-mode-tsx ()
      (when (and (buffer-file-name) (string-equal "tsx" (file-name-extension (buffer-file-name))))
        (gr/setup-tide)))
    (add-hook 'web-mode-hook 'gr/setup-web-mode-tsx)))
