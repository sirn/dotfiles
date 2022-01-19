;; -*- lexical-binding: t -*-

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
    "\\.svelte\\'"
    "\\.tpl\\.php\\'"
    "\\.tsx\\'"
    "\\.jsx?\\'")

  :init
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  :config
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.js[x]?\\'"))
  (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.cjs\\'"))

  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
      (cl-remove-if
        (lambda (item) (member (car item) types))
        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats)))

  (defun gemacs--web-js-fix-comments ()
    "Fix comment handling in `web-mode' for JavaScript."
    (when (member web-mode-content-type '("javascript" "jsx"))
      (setq-local comment-start "//")
      (setq-local comment-end "")
      (setq-local comment-start-skip "// *")))

  (add-hook 'web-mode-hook #'gemacs--web-js-fix-comments)

  (use-feature apheleia
    :preface
    (eval-when-compile
      (defvar web-mode-fontification-off))

    :config
    (defun gemacs--web-highlight-after-formatting ()
      "Make sure syntax highlighting works with Apheleia.
The problem is that `web-mode' doesn't do highlighting correctly
in the face of arbitrary buffer modifications, and kind of hacks
around the problem by hardcoding a special case for yanking based
on the value of `this-command'. So, when buffer modifications
happen in an unexpected (to `web-mode') way, we have to manually
poke it. Otherwise the modified text remains unfontified."
      (setq web-mode-fontification-off nil)
      (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
        (save-excursion
          (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end))))

    (add-hook 'apheleia-post-format-hook
      #'gemacs--web-highlight-after-formatting)))


(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))
