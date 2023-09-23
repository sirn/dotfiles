;; -*- lexical-binding: t; no-native-compile: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

;; --------------------------------------------------------------------------
;;; Key bindings

(use-feature emacs
  :general
  (leader
    "wo" #'other-window
    "wd" #'delete-window
    "wD" #'delete-other-windows
    "w-" #'split-window-below
    "w/" #'split-window-right
    "w=" #'balance-windows
    "bd" #'kill-buffer
    "bD" #'kill-buffer-and-window
    "wR" #'redraw-display))


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


(use-package ace-link
  :general
  (leader
   "jL" #'ace-link))


(use-feature delsel
  :demand t

  :config
  (delete-selection-mode +1))


(use-feature display-line-numbers
  :general
  (leader
    "Ll" #'display-line-numbers-mode
    "LL" #'global-display-line-numbers-mode))


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


(use-package envrc
  ;; envrc needs to run as late as humanly possible
  :commands envrc-global-mode

  :general
  (leader
    "e" '(:keymap envrc-command-map))

  :init
  (add-hook 'gemacs-after-init-hook 'envrc-global-mode))


;; --------------------------------------------------------------------------
;;; Minibuffers


(use-package prescient
  :demand t

  :custom
  (prescient-history-length 1000)

  :config
  (prescient-persist-mode +1)

  (use-feature emacs
    :custom
    (completion-styles '(prescient basic))))


(use-package marginalia
  :demand t

  :preface
  (eval-when-compile
    (declare-function marginalia-mode nil))

  :general
  (:keymaps 'marginalia-mode-map
   "M-A" #'marginalia-cycle)

  :config
  (marginalia-mode +1))


(use-package vertico
  :demand t

  :preface
  (eval-when-compile
    (declare-function vertico-mode nil))

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
   "C-x 4 b" #'consult-buffer-other-window
   "C-x 5 b" #'consult-buffer-other-frame
   "M-g g"   #'consult-goto-line
   "M-g M-g" #'consult-goto-line
   "M-s r"   #'consult-ripgrep
   "M-y"     #'consult-yank-pop)

  (:keymaps 'minibuffer-local-map
   "M-s" #'consult-history
   "M-r" #'consult-history)

  (leader
    "bb"  #'consult-buffer
    "wbb" #'consult-buffer-other-window
    "wbB" #'consult-buffer-other-frame
    "//"  #'consult-ripgrep
    "/g"  #'consult-grep)

  :init
  (use-feature project
    :config
    (general-with-eval-after-load 'general
      (general-define-key :keymaps 'project-prefix-map
        "g" #'consult-grep
        "/" #'consult-ripgrep)

      (add-to-list 'project-switch-commands '(consult-grep "Grep") t)
      (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep") t))))


(use-package ctrlf
  :demand t

  :custom
  (ctrlf-default-search-style 'fuzzy)

  :config
  (ctrlf-mode +1))



;; --------------------------------------------------------------------------
;;; Snippets


(use-feature abbrev)


;; --------------------------------------------------------------------------
;;; Autocompletion

(use-package corfu
  :straight (:files (:defaults "extensions/*") :includes (corfu-popupinfo corfu-echo))

  :demand t

  :custom
  (corfu-auto t)

  :init
  (global-corfu-mode +1)

  (use-feature emacs
    :custom
    (completion-cycle-threshold 3)
    (tab-always-indent 'complete)))


(use-feature corfu-popupinfo
  :after corfu

  :general
  (:keymaps 'corfu-map
   "M-n" #'corfu-popupinfo-scroll-down
   "M-p" #'corfu-popupinfo-scroll-up))


(use-feature corfu-quick
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


(use-package flycheck
  :demand t

  :general
  (leader
    "f" '(:keymap flycheck-command-map))

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
  :straight (:host github :repo "emacs-straight/eldoc")

  :demand t

  :custom
  (eldoc-echo-area-use-multiline-p nil)

  :config
  (use-feature flycheck
    :preface
    (eval-when-compile
      (declare-function gemacs--advice-disable-eldoc-on-flycheck nil))

    :config
    (defun gemacs--advice-disable-eldoc-on-flycheck
      (&rest _)
      "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
      (not (flycheck-overlay-errors-at (point))))

    (advice-add 'eldoc-display-message-no-interference-p :after-while
      #'gemacs--advice-disable-eldoc-on-flycheck)))


;; --------------------------------------------------------------------------
;;; Language Server Protocol


(use-package lsp-mode
  :general
  (leader
    "l" '(:keymap lsp-command-map))

  :custom
  (lsp-enable-snippet t)
  (lsp-file-watch-threshold nil)
  (lsp-restart 'auto-restart)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-suggest-server-download t)
  (lsp-completion-provider :none)
  (lsp-before-save-edits nil)

  :preface
  (eval-when-compile
    (declare-function gemacs--lsp-run-from-node-modules nil)
    (declare-function gemacs--lsp-setup-corfu nil)
    (declare-function gemacs--advice-lsp-mode-silence nil))

  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.git\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.hg\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp\\'")

  (defun gemacs--advice-lsp-mode-silence (format &rest args)
    "Silence needless diagnostic messages from `lsp-mode'.
This is a `:before-until' advice for several `lsp-mode' logging
functions."
    (or
      (member format `("No LSP server for %s(check *lsp-log*)."
                        "Connected to %s."
                        ,(concat
                           "Unable to calculate the languageId for current "
                           "buffer. Take a look at "
                           "lsp-language-id-configuration.")
                        ,(concat
                           "There are no language servers supporting current "
                           "mode %s registered with `lsp-mode'.")))
      (and (stringp (car args))
        (or (string-match-p "^no object for ident .+$" (car args))
          (string-match-p "^no identifier found$" (car args))))))

  (defun gemacs--lsp-run-from-node-modules (command)
    "Find LSP executables inside node_modules/.bin if present."
    (cl-block nil
      (prog1 command
        (when-let ((project-dir (locate-dominating-file default-directory "node_modules"))
                   (binary
                     (gemacs--path-join
                       project-dir "node_modules" ".bin" (car command))))
          (when (file-executable-p binary)
            (cl-return (cons binary (cdr command))))))))

  (defun gemacs--lsp-setup-corfu ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'gemacs--advice-lsp-mode-silence))

  (advice-add 'lsp-resolve-final-function :filter-return #'gemacs--lsp-run-from-node-modules)
  (add-hook 'lsp-completion-mode-hook #'gemacs--lsp-setup-corfu))


(use-package lsp-ui
  :preface
  (eval-when-compile
    (declare-function lsp-ui-sideline-apply-code-actions nil)
    (declare-function gemacs--advice-lsp-ui-apply-single-fix nil)
    (defvar lsp-ui-sideline-show-hover))

  :custom
  (lsp-ui-sideline-show-hover nil)

  :config
  (defun gemacs--advice-lsp-ui-apply-single-fix
    (orig-fun &rest args)
    "Apply code fix immediately if only one is possible."
    (gemacs-flet ((defun completing-read (prompt collection &rest args)
                    (if (= (safe-length collection) 1)
                      (car collection)
                      (apply completing-read prompt collection args))))
      (apply orig-fun args)))

  (advice-add 'lsp-ui-sideline-apply-code-actions :around
    #'gemacs--advice-lsp-ui-apply-single-fix)

  (use-feature lsp-mode
    :preface
    (eval-when-compile
      (defvar lsp-eldoc-enable-hover))

    :init
    (setq lsp-eldoc-enable-hover nil)))


(use-feature lsp-ui-doc
  :custom
  (lsp-ui-doc-winum-ignore nil)
  (lsp-ui-doc-use-childframe t)

  :preface
  (eval-when-compile
    (declare-function gemacs--advice-lsp-ui-doc-allow-multiline nil))

  :config
  (defun gemacs--advice-lsp-ui-doc-allow-multiline (func &rest args)
    "Prevent `lsp-ui-doc' from removing newlines from documentation."
    (gemacs-flet ((defun replace-regexp-in-string
                    (regexp rep string &rest args)
                    (if (equal regexp "`\\([\n]+\\)")
                      string
                      (apply replace-regexp-in-string
                        regexp rep string args))))
      (apply func args)))

  (advice-add 'lsp-ui-doc--render-buffer :around
    #'gemacs--advice-lsp-ui-doc-allow-multiline))


;; --------------------------------------------------------------------------
;;; Tree Sitter


(use-package tree-sitter
 :when (version<= emacs-version "29.0"))


(use-feature tree-sitter
  :demand t

  :preface
  (eval-when-compile
    (declare-function global-tree-sitter-mode nil)
    (declare-function tree-sitter-hl-mode nil))

  :config
  (global-tree-sitter-mode +1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :demand t

  :after tree-sitter)
