;; -*- lexical-binding: t; no-native-compile: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

;; --------------------------------------------------------------------------
;;; Key bindings

(use-feature emacs
  :general
  ("C-x C-b" #'switch-to-buffer)

  (leader
    "wo"  #'other-window
    "wd"  #'delete-window
    "wD"  #'delete-other-windows
    "w-"  #'split-window-below
    "w/"  #'split-window-right
    "w="  #'balance-windows
    "bd"  #'kill-buffer
    "bD"  #'kill-buffer-and-window
    "bb"  #'switch-to-buffer
    "wbb" #'switch-to-buffer-other-window))


;; --------------------------------------------------------------------------
;;; Editing behaviors


(use-feature emacs
  :custom
  (indent-tabs-mode nil)
  (x-alt-keysym 'meta)

  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (put 'downcase-region 'disabled nil))


(use-package avy
  :general
  (leader
   "jj" #'avy-goto-char
   "jJ" #'avy-goto-char-2
   "jl" #'avy-goto-line))


(use-package ag
  :general
  (leader
   "/" #'ag))


(use-package ace-link
  :general
  (leader
   "jL" #'ace-link))


(use-package ctrlf
  :demand t

  :custom
  (ctrlf-default-search-style 'fuzzy)

  :config
  (ctrlf-mode +1))


(use-feature delsel
  :demand t

  :config
  (delete-selection-mode +1))


(use-feature display-line-numbers-mode
  :general
  (leader
    "ll" #'display-line-numbers-mode
    "lL" #'global-display-line-numbers-mode))


(use-package undo-tree
  :demand t

  :general
  (leader
   "uv" #'undo-tree-visualize)

  :custom
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history t)

  :config
  (dolist (func '(undo-tree-load-history undo-tree-save-history))
    (advice-add func :around #'gemacs--advice-inhibit-message))

  (global-undo-tree-mode +1))


(use-feature subword
  :demand t

  :config
  (global-subword-mode +1))


(use-package visual-regexp
  :general
  ("M-%"   #'vr/query-replace
   "C-c s" #'vr/isearch-forward
   "C-c r" #'vr/isearch-backward
   "C-c R" #'vr/replace
   "C-c q" #'vr/query-replace)

  :config
  (use-feature visual-regexp-steroids
    :demand t))


(use-package visual-regexp-steroids
  :init
  (let ((repy (straight--repos-file "visual-regexp-steroids.el/regexp.py")))
    (setq vr/command-python (format "%s %s" "python3" repy))))


(use-package smartparens
  :general
  (leader
   "s>" #'sp-forward-slurp-sexp
   "s<" #'sp-backward-slurp-sexp
   "sk" #'sp-kill-whole-line
   "s(" #'sp-wrap-round
   "s{" #'sp-wrap-curly
   "sd" #'sp-splice-sexp)

  :preface
  (eval-when-compile
    (declare-function gemacs--smartparens-load nil))

  :init
  (defun gemacs--smartparens-load ()
    (use-feature smartparens-config :demand t)
    (smartparens-global-mode +1)
    (show-smartparens-global-mode +1)
    (sp-use-paredit-bindings))
  (add-hook 'prog-mode-hook #'gemacs--smartparens-load))


(use-feature smartparens-config
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-cancel-autoskip-on-backward-movement nil))


(use-feature paren
  :demand t

  :config
  (show-paren-mode +1))


(use-package parinfer-rust-mode
  :diminish parinfer-rust-mode

  :straight t

  :custom
  (parinfer-rust-auto-download -1)

  :init
  (setq parinfer-rust-library
        (no-littering-expand-var-file-name
         (concat
          (file-name-as-directory "parinfer-rust")
          (cond
           ((eq system-type 'darwin) "libparinfer_rust.dylib")
           ((eq system-type 'gnu/linux) "libparinfer_rust.so")))))

  (add-hook 'clojure-mode-hook #'parinfer-rust-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-rust-mode)
  (add-hook 'common-lisp-mode-hook #'parinfer-rust-mode)
  (add-hook 'scheme-mode-hook #'parinfer-rust-mode)
  (add-hook 'lisp-mode-hook #'parinfer-rust-mode)

  :config
  ;; Workaround for https://github.com/justinbarclay/parinfer-rust-mode/issues/40
  (defun parinfer-rust--check-version (_a _b _c _d)
    nil)

  (use-feature smartparens
    :config
    (add-hook 'parinfer-rust-mode-hook #'turn-off-smartparens-mode)))


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

  :config
  (unkillable-scratch +1))


;; --------------------------------------------------------------------------
;;; Snippets

(use-feature abbrev)


;; --------------------------------------------------------------------------
;;; Autocompletion

(use-package corfu
  :demand t

  :custom
  (corfu-auto t)

  :init
  (global-corfu-mode +1)

  (use-feature emacs
    :custom
    (completion-cycle-threshold 3)
    (tab-always-indent 'complete)))


(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")

  :after corfu

  :init
  ;; No :demand, only load when running in terminal.
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

  :general
  (leader
   "fn" #'flymake-goto-next-error
   "fp" #'flymake-goto-prev-error
   "fl" #'flymake-show-buffer-diagnostics
   "fP" #'flymake-show-project-diagnostics)

  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))


(use-package flymake-popon
  :straight (:type git :repo "https://codeberg.org/akib/emacs-flymake-popon")

  :demand t

  :after flymake

  :config
  (add-hook 'flymake-mode-hook #'flymake-popon-mode))


(use-package eldoc
  :straight (:host github :repo "emacs-straight/eldoc")

  :demand t

  :custom
  (eldoc-echo-area-use-multiline-p nil))


;; --------------------------------------------------------------------------
;;; Language Server Protocol

(use-package eglot
  :straight (:host github :repo "joaotavora/eglot")

  :general
  (leader
    "ef" #'eglot-code-actions)

  :custom
  (eglot-autoshutdown t)

  :preface
  (eval-when-compile
    (declare-function eglot-current-server nil)
    (declare-function eglot-format-buffer nil)
    (declare-function eglot-shutdown nil)
    (declare-function gemacs--advice-eglot-shutdown-project nil)
    (declare-function gemacs--eglot-format-buffer nil)
    (declare-function gemacs--eglot-organize-imports nil))

  :init
  (defun gemacs--eglot-format-buffer ()
    (eglot-format-buffer))

  (defun gemacs--eglot-organize-imports ()
    (call-interactively 'eglot-code-action-organize-imports))

  :config
  (use-feature project
    :config
    (defun gemacs--advice-eglot-shutdown-project (orig-fun &rest args)
      (let* ((pr (project-current t))
             (default-directory (project-root pr)))
        (when-let ((server (eglot-current-server)))
          (ignore-errors (eglot-shutdown server)))
        (apply orig-fun args)))

    (advice-add 'project-kill-buffers :around #'gemacs--advice-eglot-shutdown-project)))
