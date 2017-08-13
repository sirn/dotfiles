(defun custom/setup-web-mode-tsx ()
  (when (and (buffer-file-name)
             (string-equal "tsx" (file-name-extension (buffer-file-name))))
    (custom/setup-tide)))

(req-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.tsx\\'")
  :init
  (add-hook 'web-mode-hook 'custom/setup-web-mode-tsx))

(req-package emmet-mode
  :require web-mode
  :commands emmet-mode
  :diminish emmet-mode
  :init
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))
