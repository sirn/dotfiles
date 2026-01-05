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


;; Builtin: undo/redo window layouts with C-c left/right
(use-package winner
  :hook (after-init . winner-mode))


;; Builtin: repeat last command with single key after initial prefix
(use-package repeat
  :hook (after-init . repeat-mode))


;; --------------------------------------------------------------------------
;;; Editing behaviors

;; Builtin
(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (x-alt-keysym 'meta)

  :config
  (put 'downcase-region 'disabled nil))


;; Builtin: persist minibuffer history across sessions
(use-package savehist
  :custom
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :hook (after-init . savehist-mode))


;; Builtin: restore cursor position when reopening files
(use-package saveplace
  :hook (after-init . save-place-mode))


;; Builtin: auto-refresh buffers when files change on disk
(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :hook (after-init . global-auto-revert-mode))


;; Builtin: track recently opened files
(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  :hook (after-init . recentf-mode))


;; Jump to visible text using character-based decision tree
(use-package avy
  :general
  (leader
   "s j" #'avy-goto-char
   "s J" #'avy-goto-char-2
   "s l" #'avy-goto-line))


;; Jump to links in help/info buffers
(use-package ace-link
  :general
  (leader
   "s L" #'ace-link))


;; Builtin: replace selection when typing
(use-package delsel
  :hook (after-init . delete-selection-mode))


;; Builtin: show line numbers in margin
(use-package display-line-numbers
  :general
  (leader
    "T l" #'display-line-numbers-mode)

  :hook
  (prog-mode . display-line-numbers-mode))


;; Builtin: show column ruler at fill-column
(use-package display-fill-column-indicator-mode
  :general
  (leader
    "T f" #'display-fill-column-indicator-mode))


;; Builtin: visualize and cleanup whitespace
(use-package whitespace
  :general
  (leader
    "T w" #'whitespace-mode
    "e w" #'whitespace-cleanup)

  :custom
  (whitespace-style '(face tabs tab-mark spaces space-mark trailing lines-tail))

  :hook
  (before-save . whitespace-cleanup))


;; Visual undo tree browser
(use-package vundo
  :general
  (leader
   "e u" #'vundo))


;; Builtin: navigate camelCase and snake_case words
(use-package subword
  :hook (after-init . global-subword-mode))


;; Regexp replace with visual feedback
(use-package visual-regexp
  :general
  ("M-%"   #'vr/query-replace
   "C-c s" #'vr/isearch-forward
   "C-c r" #'vr/isearch-backward
   "C-c R" #'vr/replace
   "C-c q" #'vr/query-replace)

  :config
  (require 'visual-regexp-steroids))


;; Python/PCRE regex backend for visual-regexp
(use-package visual-regexp-steroids)


;; Automatic bracket pairing and navigation
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

  :hook
  (prog-mode . gemacs--smartparens-load)

  :init
  (defun gemacs--smartparens-load ()
    (require 'smartparens-config)
    (smartparens-global-mode +1)
    (show-smartparens-global-mode +1)
    (sp-use-paredit-bindings)))


;; Part of smartparens
(use-package smartparens-config
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-cancel-autoskip-on-backward-movement nil))


;; Builtin: highlight matching parentheses
(use-package paren
  :hook (after-init . show-paren-mode))


;; Automatic Lisp indentation and parentheses management
(use-package parinfer-rust-mode
  :diminish parinfer-rust-mode

  :preface
  (eval-when-compile
    (declare-function parinfer-rust-mode nil)
    (declare-function turn-off-smartparens-mode nil))

  :custom
  (parinfer-rust-auto-download -1)

  :hook
  ((clojure-mode . parinfer-rust-mode)
   (emacs-lisp-mode . parinfer-rust-mode)
   (common-lisp-mode . parinfer-rust-mode)
   (scheme-mode . parinfer-rust-mode)
   (lisp-mode . parinfer-rust-mode))

  :init
  (setq parinfer-rust-library
    (no-littering-expand-var-file-name
     (concat
      (file-name-as-directory "parinfer-rust/lib")
      (cond
       ((eq system-type 'darwin) "libparinfer_rust.dylib")
       ((eq system-type 'gnu/linux) "libparinfer_rust.so")))))

  :config
  (with-eval-after-load 'smartparens
    (add-hook 'parinfer-rust-mode-hook #'turn-off-smartparens-mode)))


;; Colorize nested parentheses by depth
(use-package rainbow-delimiters
  :preface
  (eval-when-compile
    (declare-function rainbow-delimiters-mode nil))

  :hook
  (prog-mode . rainbow-delimiters-mode))


;; Colorize color codes in buffer (e.g. #ff0000)
(use-package rainbow-mode
  :preface
  (eval-when-compile
    (declare-function rainbow-mode nil))

  :hook
  (prog-mode . rainbow-mode))


;; Auto-detect indentation style from file content
(use-package dtrt-indent)


;; Apply .editorconfig settings
(use-package editorconfig
  :hook (after-init . editorconfig-mode))


;; Part of editorconfig
(use-package editorconfig-core
  :demand t

  :preface
  (eval-when-compile
    (declare-function dtrt-indent-mode nil))

  :hook
  ((conf-mode . gemacs--dtrt-maybe-enable)
   (text-mode . gemacs--dtrt-maybe-enable)
   (prog-mode . gemacs--dtrt-maybe-enable))

  :init
  (defun gemacs--dtrt-maybe-enable ()
    "Enable `dtrt-indent-mode' if `.editorconfig' is not present"
    (when (not (and (stringp buffer-file-name)
                 (editorconfig-core-get-nearest-editorconfig
                   (file-name-directory buffer-file-name))))
      (dtrt-indent-mode))))


;; Prevent *scratch* buffer from being killed
(use-package unkillable-scratch
  :demand t

  :config
  (unkillable-scratch +1))


;; Direnv integration for per-project environments
(use-package envrc
  :commands envrc-global-mode

  :general
  (leader
    "E" '(:keymap envrc-command-map))

  :hook
  (gemacs-after-init . envrc-global-mode)

  :init
  (defun gemacs--envrc-inject-emacs-bin-deps (&rest _)
    "Injects local emacs-bin-deps"
    (add-to-list 'exec-path (expand-file-name "~/.emacs.d/var/emacs-bin-deps") t))

  :config
  (with-eval-after-load 'envrc
    (advice-add 'envrc--apply :after #'gemacs--envrc-inject-emacs-bin-deps)))


;; Use Emacs as $EDITOR for git commits, etc.
(use-package with-editor)


;; --------------------------------------------------------------------------
;;; Minibuffers

;; Builtin: allow minibuffer commands within minibuffer
(use-package emacs
  :custom
  (enable-recursive-minibuffers t))


;; Frequency/recency sorting for completion candidates
(use-package prescient
  :custom
  (prescient-history-length 1000)

  :hook (after-init . prescient-persist-mode))


;; Space-separated pattern matching for completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))


;; Rich annotations for completion candidates
(use-package marginalia
  :custom
  (marginalia-max-relative-age (* 60 60 24 7))
  (marginalia-field-width 80)
  (marginalia-separator "    ")

  :general
  (:keymaps 'minibuffer-local-map
   "M-A" #'marginalia-cycle)

  :hook (after-init . marginalia-mode))


;; Vertical completion UI in minibuffer
(use-package vertico
  :custom
  (vertico-cycle t)

  :hook (after-init . vertico-mode))


;; Prescient integration for vertico
(use-package vertico-prescient
  :after (vertico prescient)
  :hook (after-init . vertico-prescient-mode))


;; Enhanced search and navigation commands
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


;; Contextual actions on completion candidates
(use-package embark
  :general
  (leader
   "c e a" #'embark-act
   "c e e" #'embark-dwim
   "c e b" #'embark-bindings)

  (:keymaps 'minibuffer-local-map
   "C-," #'embark-export))


;; Embark integration for consult
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; --------------------------------------------------------------------------
;;; Snippets

;; Builtin: abbreviation expansion
(use-package abbrev)


;; --------------------------------------------------------------------------
;;; Autocompletion

;; Completion popup at point
(use-package corfu
  :custom
  (corfu-auto t)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)

  :hook (after-init . global-corfu-mode))


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


;; Prescient integration for corfu
(use-package corfu-prescient
  :after (corfu prescient)
  :hook (after-init . corfu-prescient-mode))


;; Corfu support in terminal
(use-package corfu-terminal
  :after corfu

  :preface
  (eval-when-compile
    (declare-function corfu-terminal-mode nil))

  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))


;; --------------------------------------------------------------------------
;;; Autoformatting

;; Run code formatters asynchronously on save
(use-package apheleia)


;; --------------------------------------------------------------------------
;;; Errors and documentation

;; Builtin: on-the-fly syntax checking
(use-package flymake
  :general
  (leader
    "c f f" #'flymake-show-buffer-diagnostics
    "c f p" #'flymake-show-project-diagnostics
    "c f n" #'flymake-goto-next-error
    "c f N" #'flymake-goto-prev-error))


;; Additional flymake backends for various languages
(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))


;; Builtin: show documentation in echo area
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))


;; --------------------------------------------------------------------------
;;; Tree Sitter (builtin treesit)

(use-package emacs
  :init
  (add-to-list
   'treesit-extra-load-path
   (no-littering-expand-var-file-name "treesit-grammars/lib")))


;; --------------------------------------------------------------------------
;;; Code folding

;; Fold code by indentation level
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

  :hook
  (prog-mode . outline-indent-minor-mode))
