;; -*- lexical-binding: t -*-

(use-package helpful
  :commands
  (helpful-callable
   helpful-variable
   helpful-symbol
   helpful-key
   helpful-function
   helpful-macro
   helpful-command
   helpful-at-point)

  :bind
  (([remap describe-function] . #'helpful-callable)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-symbol]   . #'helpful-symbol)
   ([remap describe-key]      . #'helpful-key)

   :map help-map
   ("F"   . #'helpful-function)
   ("M-f" . #'helpful-macro)
   ("C"   . #'helpful-command)

   :map global-map
   ("C-c C-d" . #'helpful-at-point))

  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))


(use-feature elisp-mode
  :config
  (defun gemacs--flycheck-elisp-setup ()
    "Disable some Flycheck checkers for Emacs Lisp."
    (gemacs--flycheck-disable-checkers 'emacs-lisp 'emacs-lisp-checkdoc))

  (defun gemacs--advice-company-elisp-use-helpful
    (func &rest args)
    "Cause `company' to use Helpful to show Elisp documentation."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function
               ((symbol-function #'describe-variable) #'helpful-variable)
               ((symbol-function #'help-buffer) #'current-buffer)))
      (apply func args)))

  (defun gemacs--advice-fill-elisp-docstrings-correctly (&rest _)
    "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
    (when (and (derived-mode-p #'emacs-lisp-mode)
            (eq (get-text-property (point) 'face) 'font-lock-doc-face))
      ""))

  (add-hook 'emacs-lisp-mode-hook #'gemacs--flycheck-elisp-setup)

  (advice-add 'elisp--company-doc-buffer :around
    #'gemacs--advice-company-elisp-use-helpful)

  (advice-add 'fill-context-prefix :before-until
    #'gemacs--advice-fill-elisp-docstrings-correctly))
