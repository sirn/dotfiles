;; -*- lexical-binding: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/raxod502/radian/blob/242c55c/emacs/radian.el

(use-package go-mode
  :preface
  (eval-when-compile
    (declare-function go--backward-irrelevant nil))

  :config
  (defvar gemacs--go-defun-regexp
    "^\\(const\\|func\\|import\\|interface\\|package\\|type\\|var\\)"
    "Regexp matching top-level declarations in Go.")

  (defun gemacs--go-beginning-of-defun (&optional arg)
    "Move to beginning of current or previous top-level declaration."
    (cond
     ((null arg)
      (cl-block nil
        (while t
          (re-search-backward gemacs--go-defun-regexp nil 'noerror)
          (when (or (bobp)
                    (eq (get-text-property (point) 'face)
                        'font-lock-keyword-face))
            (cl-return)))))
     ((> arg 0)
      (dotimes (_ arg)
        (gemacs--go-beginning-of-defun)))
     ((< arg 0)
      ;; Yuck -- but we need to implement this, otherwise
      ;; `end-of-defun' just does the wrong thing :/
      (dotimes (_ (- arg))
        (gemacs--go-beginning-of-defun)
        (gemacs--go-end-of-defun)
        (gemacs--go-end-of-defun))
      (gemacs--go-beginning-of-defun))))

  (defun gemacs--go-end-of-defun ()
    "Move to end of current or previous top-level declaration.
Only works if `gemacs--go-beginning-of-defun' was just called
previously."
    (dotimes (_ 2)
      (cl-block nil
        (while t
          (re-search-forward gemacs--go-defun-regexp nil 'noerror)
          (when (or (eobp)
                    (save-excursion
                      (beginning-of-line)
                      (eq (get-text-property (point) 'face)
                          'font-lock-keyword-face)))
            (cl-return)))))
    (beginning-of-line)
    (go--backward-irrelevant 'stop-at-string)
    (forward-line))

  (defun gemacs--go-defun-setup ()
    "Set up \\[beginning-of-defun] and \\[end-of-defun] correctly.
See <https://github.com/dominikh/go-mode.el/issues/232>."
    (setq-local beginning-of-defun-function #'gemacs--go-beginning-of-defun)
    (setq-local end-of-defun-function #'gemacs--go-end-of-defun))

  (add-hook 'go-mode-hook #'gemacs--go-defun-setup)

  (use-feature lsp-ui
    :preface
    (eval-when-compile
      (declare-function lsp-ui-sideline--code-actions nil))

    :config
    (defun gemacs--advice-lsp-ui-organize-imports-more-cleanly
      (func actions &rest args)
      "Clean up the \"Organize Imports\" code actions for Go.
Firstly, don't display \"Organize Imports\" or \"Organize All
Imports\" in the sideline, as gopls sometimes reports these code
actions when the indentation is wrong (rather than when imports
need to be changed). Secondly, filter out \"Organize All
Imports\" internally, so that applying a code action will default
to \"Organize Imports\" instead of prompting you to decide
between that and \"Organize All Imports\" (which does the same
thing as far as I can tell)."
      (let ((actions-to-keep nil)
            (actions-to-render nil))
        (dolist (action actions)
          (unless (equal "Organize All Imports" (gethash "title" action))
            (push action actions-to-keep)
            (unless (equal "Organize Imports" (gethash "title" action))
              (push action actions-to-render))))
        (setq actions-to-keep (nreverse actions-to-keep))
        (setq actions-to-render (nreverse actions-to-render))
        (when actions-to-render
          (apply func actions-to-render args))
        (setq lsp-ui-sideline--code-actions actions-to-keep)))

    (advice-add 'lsp-ui-sideline--code-actions :around
      #'gemacs--advice-lsp-ui-organize-imports-more-cleanly))

  (use-feature apheleia
    :config
    (add-to-list 'apheleia-formatters '(goimports . ("goimports")))
    (add-to-list 'apheleia-mode-alist '(go-mode . goimports))))
