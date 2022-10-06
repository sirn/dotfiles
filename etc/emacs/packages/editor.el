;; -*- lexical-binding: t; no-native-compile: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

;; --------------------------------------------------------------------------
;;; Editing behaviors

(setq-default indent-tabs-mode nil)
(setq-default x-alt-keysym 'meta)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'downcase-region 'disabled nil)

(use-package avy
  :leader
  ("jj" #'avy-goto-char
   "jJ" #'avy-goto-char-2
   "jl" #'avy-goto-line))


(use-package ag
  :preface
  (eval-when-compile
    (declare-function ag nil))

  :leader
  ("/" #'ag))


(use-package ace-link
  :leader
  ("jL" #'ace-link))


(use-package ctrlf
  :demand t

  :preface
  (eval-when-compile
    (declare-function ctrlf-mode nil)
    (defvar ctrlf-default-search-style))

  :init
  (setq ctrlf-default-search-style 'fuzzy)

  :config
  (ctrlf-mode +1))


(use-feature delsel
  :demand t
  :config
  (delete-selection-mode +1))


(use-package undo-tree
  :demand t
  :leader
  ("uv" #'undo-tree-visualize)

  :preface
  (eval-when-compile
    (declare-function global-undo-tree-mode nil))

  :init
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-auto-save-history t)

  :config
  (dolist (func '(undo-tree-load-history undo-tree-save-history))
    (advice-add func :around #'gemacs--advice-inhibit-message))

  (global-undo-tree-mode +1))


(use-feature subword
  :demand t
  :config
  (global-subword-mode +1))


(use-package visual-regexp
  :bind
  (("M-%"   . #'vr/query-replace)
   ("C-c s" . #'vr/isearch-forward)
   ("C-c r" . #'vr/isearch-backward)
   ("C-c R" . #'vr/replace)
   ("C-c q" . #'vr/query-replace))

  :config
  (use-feature visual-regexp-steroids
    :demand t))


(use-package visual-regexp-steroids
  :init
  (let ((repy (straight--repos-file "visual-regexp-steroids.el/regexp.py")))
    (setq vr/command-python (format "%s %s" "python3" repy))))


(use-package smartparens
  :demand t

  :leader
  ("s>" #'sp-forward-slurp-sexp
   "s<" #'sp-backward-slurp-sexp
   "sk" #'sp-kill-whole-line
   "s(" #'sp-wrap-round
   "s{" #'sp-wrap-curly
   "sd" #'sp-splice-sexp)

  :config
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (sp-use-paredit-bindings)

  (add-hook 'clojure-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'common-lisp-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'scheme-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'lisp-mode-hook 'turn-off-smartparens-mode))


(use-feature smartparens-config
  :demand t

  :config
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil))


(use-feature paren
  :demand t

  :config
  (show-paren-mode +1))


(use-package parinfer-rust-mode
  :diminish parinfer-rust-mode
  :straight t

  :preface
  (eval-when-compile
    (defvar parinfer-rust-library)
    (defvar parinfer-rust-auto-download))

  :init
  (setq parinfer-rust-auto-download nil)
  (setq parinfer-rust-library
        (no-littering-expand-var-file-name
         (concat
          (file-name-as-directory "parinfer-rust")
          (cond
           ((eq system-type 'darwin) "parinfer-rust-darwin.so")
           ((eq system-type 'gnu/linux) "parinfer-rust-linux.so")
           ((eq system-type 'windows-nt) "parinfer-rust-windows.dll")))))

  (add-hook 'clojure-mode-hook 'parinfer-rust-mode)
  (add-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)
  (add-hook 'common-lisp-mode-hook 'parinfer-rust-mode)
  (add-hook 'scheme-mode-hook 'parinfer-rust-mode)
  (add-hook 'lisp-mode-hook 'parinfer-rust-mode)

  :config
  ;; Workaround for https://github.com/justinbarclay/parinfer-rust-mode/issues/40
  (defun parinfer-rust--check-version (_a _b _c _d)
    nil))


(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))


(use-package dtrt-indent)


(use-package editorconfig
  :init
  (defun gemacs--editorconfig-load ()
    "Load `editorconfig' when initially finding a file."
    (require 'editorconfig)
    (remove-hook 'find-file-hook #'gemacs--editorconfig-load))
  (add-hook 'find-file-hook #'gemacs--editorconfig-load)

  :config
  (editorconfig-mode +1))


(use-feature editorconfig-core
  :demand t

  :init
  (defun gemacs--dtrt-maybe-enable ()
    "Enable `dtrt-indent-mode' if `.editorconfig' is not present"
    (when (not (and (stringp buffer-file-name)
                 (editorconfig-core-get-nearest-editorconfig
                   (file-name-directory buffer-file-name))))
      (dtrt-indent-mode)))

  (add-hook 'conf-mode-hook #'gemacs--dtrt-maybe-enable)
  (add-hook 'text-mode-hook #'gemacs--dtrt-maybe-enable)
  (add-hook 'prog-mode-hook #'gemacs--dtrt-maybe-enable))


(use-package unkillable-scratch
  :demand t

  :preface
  (eval-when-compile
    (declare-function unkillable-scratch 1))

  :config
  (unkillable-scratch +1))


;; --------------------------------------------------------------------------
;;; Snippets

(use-feature abbrev)


(use-package yasnippet
  :defer 0.5

  :preface
  (eval-when-compile
    (declare-function yas--make-control-overlay nil))

  :bind
  (:map yas-minor-mode-map
    ("TAB"   . nil)
    ("<tab>" . nil))

  :config
  (setq yas-verbosity 2)
  (yas-global-mode +1))


;; --------------------------------------------------------------------------
;;; Autocompletion

(use-package corfu
  :defer 0.5

  :preface
  (eval-when-compile
    (declare-function global-corfu-mode nil)
    (defvar corfu-auto))

  :init
  (setq corfu-auto t)
  (global-corfu-mode +1)

  (use-feature emacs
    :init
    (setq completion-cycle-threshold 3)
    (setq tab-always-indent 'complete)))


(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :demand t
  :after corfu

  :preface
  (eval-when-compile
    (declare-function corfu-terminal-mode nil))

  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))


;; --------------------------------------------------------------------------
;;; Autoformatting

(use-package apheleia
  :straight (:host github :repo "radian-software/apheleia")

  :init
  (defun gemacs--save-buffer-reformat-maybe (func &optional arg)
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  (advice-add 'save-buffer :around #'gemacs--save-buffer-reformat-maybe))


;; --------------------------------------------------------------------------
;;; Errors and documentation

(use-feature flymake
  :demand t

  :leader
  ("fn" #'flymake-goto-next-error
   "fp" #'flymake-goto-prev-error
   "fl" #'flymake-show-buffer-diagnostics
   "fP" #'flymake-show-project-diagnostics)

  :preface
  (eval-when-compile
    (declare-function flymake-goto-next-error nil)
    (declare-function flymake-goto-prev-error nil)
    (declare-function flymake-show-buffer-diagnostics nil)
    (declare-function flymake-show-project-diagnostics nil))

  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))


(use-package flymake-popon
  :straight (:type git :repo "https://codeberg.org/akib/emacs-flymake-popon")
  :demand t
  :after flymake

  :preface
  (eval-when-compile
    (declare-function flymake-popon-mode nil))

  :config
  (add-hook 'flymake-mode-hook #'flymake-popon-mode))


(use-feature eldoc
  :demand t

  :config
  (setq eldoc-echo-area-use-multiline-p nil))


;; --------------------------------------------------------------------------
;;; Language Server Protocol

(use-package eglot
  :preface
  (eval-when-compile
    (declare-function eglot-code-action-organize-imports nil)
    (declare-function eglot-format-buffer nil)
    (declare-function gemacs--eglot-format-buffer nil)
    (declare-function gemacs--eglot-organize-imports nil))

  :init
  (defun gemacs--eglot-format-buffer ()
    (eglot-format-buffer))

  (defun gemacs--eglot-organize-imports ()
    (call-interactively 'eglot-code-action-organize-imports)))
