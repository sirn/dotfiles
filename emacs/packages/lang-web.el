(use-package emmet-mode
  :after web-mode
  :commands emmet-mode
  :diminish emmet-mode
  :ensure t

  :preface
  (defvar web-mode-enable-current-element-highlight)
  (defvar web-mode-enable-current-column-highlight)

  :init
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


(use-package web-mode
  :ensure t
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
    (declare-function setup-tide nil))

  :init
  (when (boundp 'setup-tide)
    (defun setup-web-mode-tsx ()
      (when (and (buffer-file-name) (string-equal "tsx" (file-name-extension (buffer-file-name))))
        (setup-tide)))
    (add-hook 'web-mode-hook 'setup-web-mode-tsx)))
