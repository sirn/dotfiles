(use-package emacs-lisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)

  :preface
  (eval-when-compile
    (declare-function gr/emacs-lisp-enable-flycheck-maybe nil))

  :init
  (defun gr/emacs-lisp-enable-flycheck-maybe ()
    (when (and
            (stringp buffer-file-name)
            (not
              (string-match
                (rx ".dotfiles/" (* nonl))
                buffer-file-name)))
      (flycheck-mode t)))

  (add-hook 'emacs-lisp-mode-hook 'gr/emacs-lisp-enable-flycheck-maybe))
