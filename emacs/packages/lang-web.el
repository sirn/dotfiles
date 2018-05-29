(defun custom/setup-web-mode-tsx ()
  (when (and (buffer-file-name)
             (string-equal "tsx" (file-name-extension (buffer-file-name))))
    (custom/setup-tide)))

(req-package emmet-mode
  :require web-mode
  :commands emmet-mode
  :diminish emmet-mode
  :init
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode))
  :config
  (progn
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)))

(req-package web-mode
  :mode ("\\.phtml\\'"
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
  :init
  (add-hook 'web-mode-hook 'custom/setup-web-mode-tsx))
