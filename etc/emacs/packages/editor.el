;; -*- lexical-binding: t -*-

;; Note:
;; Many parts of this file are taken from Radian with some modifications.
;; https://github.com/raxod502/radian/blob/242c55c/emacs/radian.el

;; --------------------------------------------------------------------------
;;; Editing behaviors

(setq-default indent-tabs-mode nil)
(setq-default x-alt-keysym 'meta)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'downcase-region 'disabled nil)

(use-package avy
  :commands
  (avy-goto-char
   avy-goto-char-2
   avy-goto-line)

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
  :commands ace-link

  :leader
  ("jL" #'ace-link))


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
  :commands
  (vr/replace
   vr/query-replace
   vr/isearch-forward)

  :bind
  (("M-%"   . #'vr/query-replace)
   ("C-c s" . #'vr/isearch-forward)
   ("C-c r" . #'vr/replace)
   ("C-c q" . #'vr/query-replace)))


(use-package visual-regexp-steroids
  :init
  (let ((repy (straight--repos-file "visual-regexp-steroids.el/regexp.py")))
    (setq vr/command-python (format "%s %s" "python3" repy))))


(use-package origami
  :demand t

  :config
  (global-origami-mode +1)
  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map
      (kbd "TAB")   #'origami-toggle-node
      (kbd "M-TAB") #'origami-toggle-all-nodes)))


(use-package smartparens
  :demand t

  :config
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (sp-use-paredit-bindings)

  :leader
  ("s>" #'sp-forward-slurp-sexp
   "s<" #'sp-backward-slurp-sexp
   "sk" #'sp-kill-whole-line
   "s(" #'sp-wrap-round
   "s{" #'sp-wrap-curly
   "sd" #'sp-splice-sexp)

  :config
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


(use-package parinfer-rust-mode
  :commands parinfer-rust-mode
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
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))


(use-package dtrt-indent
  :commands dtrt-indent-mode)


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
  :preface
  (eval-when-compile
    (declare-function yas--make-control-overlay nil))

  :bind
  (:map yas-minor-mode-map
    ("TAB"   . nil)
    ("<tab>" . nil))

  :config
  (setq yas-verbosity 2)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active.
  (use-feature company
    :config

    (defun gemacs--yasnippet-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
        event
        (vector event)))

    (defvar gemacs--yasnippet-then-company-keymap
      (let ((keymap (copy-keymap yas-keymap)))
        (map-keymap
          (lambda (event company-cmd)
            (let* ((event (gemacs--yasnippet-normalize-event event))
                   (yas-cmd (lookup-key yas-keymap event)))
              (define-key keymap event
                `(menu-item
                   nil ,company-cmd :filter
                   (lambda (cmd)
                     (if company-my-keymap
                       ',company-cmd
                       ',yas-cmd))))))
          company-active-map)
        keymap)
      "Keymap which delegates to both `company-active-map' and `yas-keymap'.")

    (defun gemacs--advice-company-overrides-yasnippet
      (yas--make-control-overlay &rest args)
      "Allow `company' keybindings to override those of `yasnippet'."
      (let ((yas-keymap gemacs--yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args)))

    (advice-add 'yas--make-control-overlay :around
      #'gemacs--advice-company-overrides-yasnippet)))


;; --------------------------------------------------------------------------
;;; Autocompletion

(defvar-local gemacs--company-buffer-modified-counter nil
  "Last return value of `buffer-chars-modified-tick'.
Used to ensure that Company only initiates a completion when the
buffer is modified.")

(use-package company
  :defer 0.5

  :preface
  (eval-when-compile
    (declare-function company-explicit-action-p nil)
    (declare-function company-complete-common-or-cycle nil)
    (declare-function company-select-previous-or-abort nil)
    (declare-function company-select-next-or-abort nil)
    (declare-function company-complete-selection nil)
    (declare-function company-next-page nil)
    (declare-function company-previous-page nil)
    (declare-function company-select-previous nil)
    (declare-function company-select-next nil)
    (declare-function company--should-begin nil))

  :bind
  (([remap completion-at-point] . #'company-manual-begin)
   ([remap complete-symbol]     . #'company-manual-begin)

   :map company-active-map
   ([tab]     . #'company-complete-common-or-cycle)
   ("TAB"     . #'company-complete-common-or-cycle)
   ("S-TAB"   . #'company-select-previous-or-abort)
   ([backtab] . #'company-select-previous-or-abort)
   ([S-tab]   . #'company-select-previous-or-abort)
   ("C-p"     . #'company-select-previous-or-abort)
   ("C-n"     . #'company-select-next-or-abort)
   ("C-l"     . #'company-complete-selection)
   ("C-v"     . #'company-next-page)
   ("M-v"     . #'company-previous-page)
   ("C-s"     . nil)

   :filter (company-explicit-action-p)
   ("<return>" . #'company-complete-selection)
   ("RET"      . #'company-complete-selection)
   ("<up>"     . #'company-select-previous)
   ("<down>"   . #'company-select-next))

  :bind*
  (("M-TAB"    . #'company-manual-begin))

  :config
  (setq company-idle-delay 0.15)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-minimum company-tooltip-limit)
  (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-show-quick-access t)
  (setq company-require-match #'company-explicit-action-p)
  (setq company-auto-commit-chars nil)
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t)

  (defun gemacs--advice-company-complete-on-change ()
    "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick gemacs--company-buffer-modified-counter)
        ;; Only trigger completion if previous counter value was
        ;; non-nil (i.e., don't trigger completion just as we're
        ;; jumping to a buffer for the first time).
        (prog1 gemacs--company-buffer-modified-counter
          (setq gemacs--company-buffer-modified-counter tick)))))

  (advice-add 'company--should-begin :override
    #'gemacs--advice-company-complete-on-change)

  (global-company-mode +1))


(use-package company-prescient
  :demand t
  :after prescient

  :preface
  (eval-when-compile
    (declare-function company-prescient-mode nil))

  :config
  (company-prescient-mode +1))


;; --------------------------------------------------------------------------
;;; Autoformatting

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :commands apheleia-global-mode

  :init
  (defun gemacs--save-buffer-reformat-maybe (func &optional arg)
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  (advice-add 'save-buffer :around #'gemacs--save-buffer-reformat-maybe)
  (apheleia-global-mode +1))


;; --------------------------------------------------------------------------
;;; Errors and documentation

(use-package flycheck
  :defer 4
  :commands (flycheck-list-errors)

  :bind-keymap (("C-c !" . #'flycheck-command-map))

  :leader
  ("fp" #'flycheck-previous-error
   "fn" #'flycheck-next-error
   "fl" #'flycheck-list-errors)

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
  (global-flycheck-mode +1)
  (dolist (name '("python" "python2" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

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


(use-feature eldoc
  :demand t

  :config
  (gemacs-when-compiletime (version<= "27" emacs-version)
    (el-patch-defun eldoc-print-current-symbol-info ()
      (el-patch-concat
        "Print the text produced by `eldoc-documentation-function'."
        (el-patch-add "\nDon't trample on existing messages."))
      ;; This is run from post-command-hook or some idle timer thing,
      ;; so we need to be careful that errors aren't ignored.
      (with-demoted-errors "eldoc error: %s"
        (if (not (eldoc-display-message-p))
          ;; Erase the last message if we won't display a new one.
          (when eldoc-last-message
            (el-patch-swap
              (eldoc-message nil)
              (setq eldoc-last-message nil)))
          (let ((non-essential t))
            ;; Only keep looking for the info as long as the user
            ;; hasn't requested our attention.  This also locally
            ;; disables inhibit-quit.
            (while-no-input
              (eldoc-message (funcall eldoc-documentation-function))))))))

  (gemacs-when-compiletime (and (version< emacs-version "27")
                             (version<= "26" emacs-version))
    (el-patch-defun eldoc-print-current-symbol-info ()
      (el-patch-concat
        "Print the text produced by `eldoc-documentation-function'."
        (el-patch-add "\nDon't trample on existing messages."))
      ;; This is run from post-command-hook or some idle timer thing,
      ;; so we need to be careful that errors aren't ignored.
      (with-demoted-errors "eldoc error: %s"
        (and (or (eldoc-display-message-p)
               ;; Erase the last message if we won't display a new one.
               (when eldoc-last-message
                 (el-patch-swap
                   (eldoc-message nil)
                   (setq eldoc-last-message nil))
                 nil))
          (eldoc-message (funcall eldoc-documentation-function))))))

  (gemacs-when-compiletime (version< emacs-version "26")
    (el-patch-defun eldoc-print-current-symbol-info ()
      ;; This is run from post-command-hook or some idle timer thing,
      ;; so we need to be careful that errors aren't ignored.
      (with-demoted-errors "eldoc error: %s"
        (and (or (eldoc-display-message-p)
               ;; Erase the last message if we won't display a new one.
               (when eldoc-last-message
                 (el-patch-swap
                   (eldoc-message nil)
                   (setq eldoc-last-message nil))
                 nil))
          (eldoc-message (funcall eldoc-documentation-function))))))

  (setq eldoc-echo-area-use-multiline-p nil)

  (use-feature flycheck
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
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-file-watch-threshold nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-headerline-breadcrumb-enable nil)

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

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'gemacs--advice-lsp-mode-silence))

  (advice-add 'lsp-resolve-final-function :filter-return
    #'gemacs--lsp-run-from-node-modules))


(use-package lsp-ui
  :bind (("C-c f" . #'lsp-ui-sideline-apply-code-actions))

  :preface
  (eval-when-compile
    (declare-function lsp-ui-sideline-apply-code-actions nil)
    (declare-function gemacs--advice-lsp-ui-apply-single-fix nil)
    (defvar lsp-ui-sideline-show-hover))

  :init
  (setq lsp-ui-sideline-show-hover nil)

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
    :init
    (setq lsp-eldoc-enable-hover nil)))


(use-feature lsp-ui-doc
  :preface
  (eval-when-compile
    (declare-function gemacs--advice-lsp-ui-doc-allow-multiline nil))

  :init
  (setq lsp-ui-doc-winum-ignore nil)
  (setq lsp-ui-doc-use-childframe nil)

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


(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :preface
  (eval-when-compile
    (declare-function lsp-treemacs-sync-mode nil))

  :config
  (lsp-treemacs-sync-mode t))
