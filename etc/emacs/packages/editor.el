;; -*- lexical-binding: t; no-native-compile: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

;; --------------------------------------------------------------------------
;;; Key bindings

;; Builtin
(use-package emacs
  :general
  (leader
    "b k" #'kill-buffer
    "b r" #'revert-buffer
    "b S" #'gemacs--save-all-buffers
    "b R" #'gemacs--revert-all-buffers
    "f F" #'find-file
    "w-" #'split-window-below
    "w/" #'split-window-right
    "w=" #'balance-windows
    "wD" #'delete-other-windows
    "wd" #'delete-window
    "wR" #'redraw-display)

  :init
  (defun gemacs--save-all-buffers ()
    "Save all modified buffers without confirmation."
    (interactive)
    (save-some-buffers t))

  (defun gemacs--revert-all-buffers ()
    "Revert all file buffers without confirmation.
Buffers visiting files are reverted without confirmation.
Other buffers are left alone."
    (interactive)
    (let ((buffers (buffer-list))
          (count 0))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (when (and buffer-file-name (file-exists-p buffer-file-name))
            (let ((revert-without-query '(".*")))
              (revert-buffer t t t))
            (setq count (1+ count)))))
      (message "Reverted %d buffer(s)" count))))


;; --------------------------------------------------------------------------
;;; Editing behaviors

;; Builtin
(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (x-alt-keysym 'meta)

  :config
  (put 'downcase-region 'disabled nil))


(use-package avy
  :general
  (leader
   "s j" #'avy-goto-char
   "s J" #'avy-goto-char-2
   "s l" #'avy-goto-line))


(use-package ace-link
  :general
  (leader
   "s L" #'ace-link))


;; Builtin
(use-package delsel
  :demand t

  :config
  (delete-selection-mode +1))


;; Builtin
(use-package display-line-numbers
  :general
  (leader
    "T l" #'display-line-numbers-mode)

  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))


;; Builtin
(use-package display-fill-column-indicator-mode
  :general
  (leader
    "T f" #'display-fill-column-indicator-mode)

  :init
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))


;; Builtin
(use-package whitespace
  :general
  (leader
    "t w" #'whitespace-mode
    "e w" #'whitespace-cleanup)

  :custom
  (whitespace-style '(face tabs tab-mark spaces space-mark trailing lines-tail))

  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))


(use-package undo-tree
  :demand t

  :general
  (leader
   "e u" #'undo-tree-visualize)

  :custom
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history t)

  :config
  (dolist (func '(undo-tree-load-history undo-tree-save-history))
    (advice-add func :around #'gemacs--advice-inhibit-message))

  (global-undo-tree-mode +1))


;; Builtin
(use-package subword
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
  (require 'visual-regexp-steroids))


(use-package visual-regexp-steroids)


(use-package smartparens
  :general
  (leader
   "e x >" #'sp-forward-slurp-sexp
   "e x <" #'sp-backward-slurp-sexp
   "e x k" #'sp-kill-whole-line
   "e x (" #'sp-wrap-round
   "e x {" #'sp-wrap-curly
   "e x d" #'sp-splice-sexp)

  :preface
  (eval-when-compile
    (declare-function gemacs--smartparens-load nil)
    (declare-function sp-use-paredit-bindings nil)
    (declare-function show-smartparens-global-mode nil)
    (declare-function smartparens-global-mode nil))

  :init
  (defun gemacs--smartparens-load ()
    (require 'smartparens-config)
    (smartparens-global-mode +1)
    (show-smartparens-global-mode +1)
    (sp-use-paredit-bindings))
  (add-hook 'prog-mode-hook #'gemacs--smartparens-load))


;; Part of smartparens
(use-package smartparens-config
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-cancel-autoskip-on-backward-movement nil))


;; Builtin
(use-package paren
  :demand t

  :config
  (show-paren-mode +1))


(use-package parinfer-rust-mode
  :diminish parinfer-rust-mode

  :preface
  (eval-when-compile
    (declare-function parinfer-rust-mode nil)
    (declare-function turn-off-smartparens-mode nil))

  :custom
  (parinfer-rust-auto-download -1)

  :init
  (setq parinfer-rust-library
    (no-littering-expand-var-file-name
     (concat
      (file-name-as-directory "parinfer-rust/lib")
      (cond
       ((eq system-type 'darwin) "libparinfer_rust.dylib")
       ((eq system-type 'gnu/linux) "libparinfer_rust.so")))))

  (add-hook 'clojure-mode-hook #'parinfer-rust-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-rust-mode)
  (add-hook 'common-lisp-mode-hook #'parinfer-rust-mode)
  (add-hook 'scheme-mode-hook #'parinfer-rust-mode)
  (add-hook 'lisp-mode-hook #'parinfer-rust-mode)

  :config
  (with-eval-after-load 'smartparens
    (add-hook 'parinfer-rust-mode-hook #'turn-off-smartparens-mode)))


(use-package rainbow-delimiters
  :preface
  (eval-when-compile
    (declare-function rainbow-delimiters-mode nil))

  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package rainbow-mode
  :preface
  (eval-when-compile
    (declare-function rainbow-mode nil))

  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))


(use-package dtrt-indent)


(use-package editorconfig
  :preface
  (eval-when-compile
    (declare-function editorconfig-mode nil))

  :init
  (defun gemacs--editorconfig-load ()
    "Load `editorconfig' when initially finding a file."
    (require 'editorconfig)
    (remove-hook 'find-file-hook #'gemacs--editorconfig-load))
  (add-hook 'find-file-hook #'gemacs--editorconfig-load)

  :config
  (editorconfig-mode +1))


;; Part of editorconfig
(use-package editorconfig-core
  :demand t

  :preface
  (eval-when-compile
    (declare-function dtrt-indent-mode nil))

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


(use-package envrc
  ;; envrc needs to run as late as humanly possible
  :commands envrc-global-mode

  :general
  (leader
    "E" '(:keymap envrc-command-map))

  :init
  (defun gemacs--envrc-inject-emacs-bin-deps (&rest _)
    "Injects local emacs-bin-deps"
    (add-to-list 'exec-path (expand-file-name "~/.emacs.d/var/emacs-bin-deps") t))

  (add-hook 'gemacs-after-init-hook 'envrc-global-mode)

  :config
  (with-eval-after-load 'envrc
    (advice-add 'envrc--apply :after #'gemacs--envrc-inject-emacs-bin-deps)))


(use-package with-editor
  :demand t)


;; --------------------------------------------------------------------------
;;; Minibuffers

(use-package prescient
  :demand t

  :custom
  (prescient-history-length 1000)

  :config
  (prescient-persist-mode +1))


(use-package orderless
  :demand t

  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))


(use-package marginalia
  :demand t

  :preface
  (eval-when-compile
    (declare-function marginalia-mode nil))

  :custom
  (marginalia-max-relative-age (* 60 60 24 7))
  (marginalia-field-width 80)
  (marginalia-separator "    ")

  :general
  (:keymaps 'minibuffer-local-map
   "M-A" #'marginalia-cycle)

  :config
  (marginalia-mode +1))


(use-package vertico
  :demand t

  :preface
  (eval-when-compile
    (declare-function vertico-mode nil))

  :custom
  (vertico-cycle t)

  :config
  (vertico-mode +1))


(use-package vertico-prescient
  :after (vertico prescient)

  :demand t

  :preface
  (eval-when-compile
    (declare-function vertico-prescient-mode nil))

  :config
  (vertico-prescient-mode +1))


(use-package consult
  :general
  ("C-x C-b" #'consult-buffer
   "C-x C-f" #'consult-fd
   "M-g g"   #'consult-goto-line
   "M-g M-g" #'consult-goto-line
   "M-g i"   #'consult-imenu
   "M-g I"   #'consult-imenu-multi
   "M-s r"   #'consult-ripgrep
   "M-y"     #'consult-yank-pop)

  (:keymaps 'minibuffer-local-map
   "M-s" #'consult-history
   "M-r" #'consult-history)

  (leader
    "f f" #'consult-fd
    "f o" #'consult-outline
    "b b" #'consult-buffer
    "w b" #'consult-buffer-other-window
    "w B" #'consult-buffer-other-frame
    "s s" #'consult-ripgrep)

  :custom
  (consult-fd-args '((if (locate-dominating-file default-directory ".git")
                         '("fd" "--full-path" "--color=never")
                       '("fd" "--full-path" "--color=never" "--no-require-git")))))


(use-package embark
  :demand t

  :general
  (leader
   "c e a" #'embark-act
   "c e e" #'embark-dwim
   "c e b" #'embark-bindings)

  (:keymaps 'minibuffer-local-map
   "C-," #'embark-export))


(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(use-package ctrlf
  :demand t

  :custom
  (ctrlf-default-search-style 'fuzzy)

  :config
  (ctrlf-mode +1))



;; --------------------------------------------------------------------------
;;; Snippets

;; Builtin
(use-package abbrev)


;; --------------------------------------------------------------------------
;;; Autocompletion

(use-package corfu
  :demand t

  :preface
  (eval-when-compile
    (declare-function global-corfu-mode nil))

  :custom
  (corfu-auto t)

  :init
  (global-corfu-mode +1)

  (with-eval-after-load 'emacs
    (setq completion-cycle-threshold 3)
    (setq tab-always-indent 'complete)))


;; Part of corfu
(use-package corfu-popupinfo
  :after corfu

  :general
  (:keymaps 'corfu-map
   "M-n" #'corfu-popupinfo-scroll-down
   "M-p" #'corfu-popupinfo-scroll-up))


;; Part of corfu
(use-package corfu-quick
  :after corfu

  :general
  (:keymaps 'corfu-map
   "M-q" #'corfu-quick-complete
   "C-q" #'corfu-quick-insert))


(use-package corfu-prescient
  :after (corfu prescient)

  :demand t

  :preface
  (eval-when-compile
    (declare-function corfu-prescient-mode nil))

  :config
  (corfu-prescient-mode +1))


(use-package corfu-terminal
  :after corfu

  :preface
  (eval-when-compile
    (declare-function corfu-terminal-mode nil))

  :init
  ;; No :demand, only load when running in terminal.
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))


;; --------------------------------------------------------------------------
;;; Autoformatting

(use-package apheleia)


;; --------------------------------------------------------------------------
;;; Errors and documentation

(use-package flycheck
  :general
  (leader
    "c f" '(:keymap flycheck-command-map))

  (:keymaps 'flycheck-command-map
   "f" #'flycheck-list-errors)

  :preface
  (eval-when-compile
    (declare-function flycheck-previous-error nil)
    (declare-function flycheck-next-error nil)
    (declare-function flycheck-list-errors nil)
    (declare-function flycheck-overlay-errors-at nil)
    (declare-function flycheck-error-line-region nil))

  :init
  (defun gemacs--flycheck-disable-checkers (&rest checkers)
    "Disable the given Flycheck syntax CHECKERS, symbols.
This function affects only the current buffer, and neither causes
nor requires Flycheck to be loaded."
    (unless (boundp 'flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers nil))
    (make-local-variable 'flycheck-disabled-checkers)
    (dolist (checker checkers)
      (cl-pushnew checker flycheck-disabled-checkers)))

  :config
  ;; Run a syntax check when changing buffers, just in case you
  ;; modified some other files that impact the current one. See
  ;; https://github.com/flycheck/flycheck/pull/1308.
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to only briefly. This allows "refreshing" the syntax
  ;; check state for several buffers quickly after e.g. changing a
  ;; config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.2))




(use-package eldoc
  :demand t

  :preface
  (eval-when-compile
    (declare-function gemacs--advice-disable-eldoc-on-flycheck nil))

  :custom
  (eldoc-echo-area-use-multiline-p nil)

  :config
  (defun gemacs--advice-disable-eldoc-on-flycheck
    (&rest _)
    "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
    (not (flycheck-overlay-errors-at (point))))

  (with-eval-after-load 'flycheck
    (advice-add 'eldoc-display-message-no-interference-p :after-while
      #'gemacs--advice-disable-eldoc-on-flycheck)))


;; --------------------------------------------------------------------------
;;; Tree Sitter

(use-package tree-sitter
  :defer 2

  :preface
  (eval-when-compile
    (declare-function global-tree-sitter-mode nil)
    (declare-function tree-sitter-hl-mode nil))

  :init
  (add-to-list
   'treesit-extra-load-path
   (no-littering-expand-var-file-name "treesit-grammars/lib"))

  :config
  (global-tree-sitter-mode +1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; --------------------------------------------------------------------------
;;; Code folding

(use-package outline-indent
  :general
  (leader
    "T o" #'outline-indent-minor-mode
    "z z" #'outline-toggle-children
    "z c" #'outline-hide-subtree
    "z o" #'outline-show-subtree
    "z r" #'outline-show-all
    "z m" #'outline-hide-body
    "z n" #'outline-next-visible-heading
    "z p" #'outline-previous-visible-heading)

  :preface
  (eval-when-compile
    (declare-function outline-indent-minor-mode nil))

  :init
  (add-hook 'prog-mode-hook #'outline-indent-minor-mode))
