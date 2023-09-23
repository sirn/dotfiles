;; -*- lexical-binding: t; no-native-compile: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

(use-package go-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--go-auto-format nil)
    (declare-function gemacs--go-beginning-of-defun nil)
    (declare-function gemacs--go-defun-setup nil)
    (declare-function gemacs--go-end-of-defun nil)
    (declare-function go--backward-irrelevant nil))

  :config
  (use-feature flycheck-golangci-lint
    :preface
    (eval-when-compile
      (declare-function flycheck-golangci-lint-setup nil))

    :init
    (add-hook 'go-mode-hook #'flycheck-golangci-lint-setup))

  (use-feature lsp-mode
    :demand t

    :config
    (defun gemacs--go-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports))

    (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'go-mode-hook #'gemacs--go-auto-format))

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

  (add-hook 'go-mode-hook #'gemacs--go-defun-setup))


(use-package flycheck-golangci-lint)
