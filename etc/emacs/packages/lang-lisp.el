;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package helpful
  :general
  ([remap describe-function] #'helpful-callable
   [remap describe-variable] #'helpful-variable
   [remap describe-symbol]   #'helpful-symbol
   [remap describe-key]      #'helpful-key)

  (:keymaps 'help-map
   "F"   #'helpful-function
   "M-f" #'helpful-macro
   "C"   #'helpful-command)

  (:keymaps 'global-map
   "C-c C-d" #'helpful-at-point))


(use-feature elisp-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--advice-elisp-fill-docstrings nil))

  :config
  (defun gemacs--advice-elisp-fill-docstrings (&rest _)
    "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
    (when (and (derived-mode-p #'emacs-lisp-mode)
            (eq (get-text-property (point) 'face) 'font-lock-doc-face))
      ""))

  (advice-add 'fill-context-prefix :before-until
    #'gemacs--advice-elisp-fill-docstrings))
