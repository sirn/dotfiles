;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package helpful
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
   ("C-c C-d" . #'helpful-at-point)))


(use-feature elisp-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--elisp-flycheck-setup nil)
    (declare-function gemacs--elisp-auto-format nil)
    (declare-function gemacs--advice-elisp-fill-docstrings nil)
    (declare-function gemacs--advice-elisp-company-use-helpful nil))

  :config
  (defun gemacs--elisp-flycheck-setup ()
    "Disable some Flycheck checkers for Emacs Lisp."
    (gemacs--flycheck-disable-checkers 'emacs-lisp 'emacs-lisp-checkdoc))

  (defun gemacs--elisp-auto-format ()
      (use-feature apheleia
        :config
        (apheleia-mode -1)))

  (defun gemacs--advice-elisp-fill-docstrings (&rest _)
    "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
    (when (and (derived-mode-p #'emacs-lisp-mode)
            (eq (get-text-property (point) 'face) 'font-lock-doc-face))
      ""))

  (advice-add 'fill-context-prefix :before-until
    #'gemacs--advice-elisp-fill-docstrings)

  (use-feature helpful
    :demand t
    :config
    (defun gemacs--advice-elisp-company-use-helpful
        (func &rest args)
        "Cause `company' to use Helpful to show Elisp documentation."
        (cl-letf (((symbol-function #'describe-function) #'helpful-function
                     ((symbol-function #'describe-variable) #'helpful-variable)
                     ((symbol-function #'help-buffer) #'current-buffer)))
            (apply func args)))

    (add-hook 'emacs-lisp-mode-hook #'gemacs--elisp-flycheck-setup)
    (add-hook 'emacs-lisp-mode-hook #'gemacs--elisp-auto-format)

    (advice-add 'elisp--company-doc-buffer :around
        #'gemacs--advice-elisp-company-use-helpful)))
