;;; dot.el -- Personal Dotfiles  -*- lexical-binding: t -*-

;;; Commentary:

;; This is a personal Emacs configuration focusing on maintainability
;; and simplicity. Many stuff are copied from several sources.
;;
;; https://github.com/syl20bnr/spacemacs
;; https://github.com/raxod502/radian
;;
;; Especially Radian, which is amazing and you should use it. Many
;; stuff in this file (prefixed `radian-') are copied from Radian
;; with little or no modifications.
;;
;; I couldn't use Radian because I already maintain my own dotfiles
;; with a personalized bootstrap scripts.

;;; Code:

;;; Pre-init

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(defgroup gemacs nil
  "Gemacs's customization group."
  :prefix "gemacs-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/sirn/dotfiles"))

(defgroup radian nil
  "Radian Emacs functions"
  :prefix "radian-"
  :group 'radian
  :link '(url-link :tag "GitHub" "https://github.com/raxod502"))

;; Disable `load-prefer-newer' from init.el to optimize loading time.

(setq load-prefer-newer nil)


;;; Package management

;; Bootstrap the package manager, `straight.el'. straight.el is also used
;; to bootstrap `use-package', a macro around with-eval-after-load,
;; and `el-patch', for dynamically patching a package.

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'el-patch)

(eval-when-compile
  (defvar straight-use-package-by-default)
  (defvar use-package-always-defer))

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(eval-when-compile
  (require 'use-package)
  (require 'el-patch))

;; Also use straight to load org-mode right before anything else to
;; make sure nothing depends on Emacs-provided outdated version of org.

(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;; Feature `bind-key' provides support for rebinding keys; usually this
;; is loaded by use-package, but they are not loaded in some situation
;; (e.g. loading an byte-compiled elisp code)

(use-package bind-key
  :demand t
  :straight nil)

;; A variant of use-package that doesn't attempt to load from straight.
;;
;; Taken from Radian
;; https://github.com/raxod502/radian/blob/242c55/emacs/radian.el#L550-L556

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; An extension to use-package to provides `:leader' keyword for defining
;; `evil-leader' key bindings. Only run during compilation.

(eval-when-compile
  (defun use-package-normalize/:leader (_name-symbol keyword args)
    "Normalize `:leader' keyword in `use-package'."
    (use-package-only-one (symbol-name keyword) args
      (lambda (_label arg)
        (cond
         ((listp arg) arg)
         (t
          (use-package-error
           ":leader wants a list"))))))

  (defun use-package-handler/:leader (name-symbol _keyword arg rest state)
    "Handler for `:leader' keyword in `use-package'."
    (use-package-concat
     (use-package-process-keywords name-symbol rest state)
     `(,(macroexpand
         `(with-eval-after-load 'evil-leader
            (evil-leader/set-key ,@arg))))))

  (add-to-list 'use-package-keywords :leader))


;;; Utilities

(defun radian--path-join (path &rest segments)
  "Join PATH with SEGMENTS using `expand-file-name'.
First `expand-file-name' is called on the first member of
SEGMENTS, with PATH as DEFAULT-DIRECTORY. Then `expand-file-name'
is called on the second member, with the result of the first call
as DEFAULT-DIRECTORY, and so on. If no SEGMENTS are passed, the
return value is just PATH."
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

(defmacro radian-if-compiletime (cond then else)
  "Like `if', but COND is evaluated at compile time.
The macro expands directly to either THEN or ELSE, and the other
branch is not compiled. This can be helpful to deal with code
that uses functions only defined in a specific Emacs version."
  (declare (indent 2))
  (if (eval cond)
      then
    else))

(defmacro radian-when-compiletime (cond &rest body)
  "Like `when', but COND is evaluated at compile time.
BODY is only compiled if COND evaluates to non-nil. This can be
helpful to deal with code that uses functions only defined in a
specific Emacs version."
  (declare (indent 1))
  (when (eval cond)
    `(progn ,@body)))

(defmacro radian-protect-macros (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
This macro should be used in the following situation:
Some form is being evaluated, and this form contains as a
sub-form some code that will not be evaluated immediately, but
will be evaluated later. The code uses a macro that is not
defined at the time the top-level form is evaluated, but will be
defined by time the sub-form's code is evaluated. This macro
handles its arguments in some way other than evaluating them
directly. And finally, one of the arguments of this macro could
be interpreted itself as a macro invocation, and expanding the
invocation would break the evaluation of the outer macro.
You might think this situation is such an edge case that it would
never happen, but you'd be wrong, unfortunately. In such a
situation, you must wrap at least the outer macro in this form,
but can wrap at any higher level up to the top-level form."
  (declare (indent 0))
  `(eval '(progn ,@body)))

(defmacro radian-flet (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.
\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
		 (lambda (binding)
		   (when (memq (car binding) '(defun lambda))
		     (setq binding (cdr binding)))
		   (cl-destructuring-bind (name arglist &rest body) binding
		     (list
		      `(,name (symbol-function #',name))
		      `((symbol-function #',name)
			(lambda ,arglist
			  ,@body)))))
		 bindings))
     ,@body))

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

(defmacro radian--with-silent-message (regexps &rest body)
  "Silencing any messages that match REGEXPS, execute BODY.
REGEXPS is a list of strings; if `message' would display a
message string (not including the trailing newline) matching any
element of REGEXPS, nothing happens. The REGEXPS need not match
the entire message; include ^ and $ if necessary. REGEXPS may
also be a single string."
  (declare (indent 1))
  (let ((regexps-sym (cl-gensym "regexps")))
    `(let ((,regexps-sym ,regexps))
       (when (stringp ,regexps-sym)
         (setq ,regexps-sym (list ,regexps-sym)))
       (radian-flet ((defun message (format &rest args)
                       (let ((str (apply #'format format args)))
                         ;; Can't use an unnamed block because during
                         ;; byte-compilation, some idiot loads `cl', which
                         ;; sticks an advice onto `dolist' that makes it
                         ;; behave like `cl-dolist' (i.e., wrap it in
                         ;; another unnamed block) and therefore breaks
                         ;; this code.
                         (cl-block done
                           (dolist (regexp ,regexps-sym)
                             (when (or (null regexp)
                                       (string-match-p regexp str))
                               (cl-return-from done)))
                           (funcall message "%s" str)))))
         ,@body))))

(defun radian--advice-silence-messages (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages.
This is an `:override' advice for many different functions."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))


;;; Configure ~/.emacs.d paths

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.

(use-package no-littering
  :demand t

  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))


;;; Auth and Security

;; Feature `gnutls' provides support for SSL/TLS connections, using
;; the GnuTLS library.

(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))

  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 3072)
  (when (< emacs-major-version 27)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; Feature `epg' provides an interface for GnuPG.

(use-feature epg)

;; Feature `epa' provides an integration between Emacs and GnuPG
;; programs.

(use-feature epa
  :after epg
  :init
  (setq epa-pinentry-mode 'loopback))

;; Package `pinentry' allows Emacs minibuffer to be used as pinentry input
;; to GnuPG programs.

(use-package pinentry
  :demand t
  :after epg

  :config
  ;; Allow gpg-connect-agent in ssh-agent mode to forward pinentry to Emacs
  ;; since the ssh-agent protocol has no way to pass the TTY to gpg-agent.
  ;; See also --enable-ssh-support in gpg-agent(1)
  ;;
  ;; gpg-agent use INSIDE_EMACS environment variable to detect that we're
  ;; running in Emacs, but the environment variable is set in term-mode.
  ;; We do it here so all pinentry actually goes to Emacs.
  ;;
  ;; Also this hook has a nice effect of auto-starting gpg-agent when
  ;; needed by ssh.
  (setenv "INSIDE_EMACS" emacs-version)

  (defun gemacs--gpg-update-tty (&rest _args)
    (shell-command
     "gpg-connect-agent updatestartuptty /bye"
     " *gpg-update-tty*"))

  (pinentry-start))

;; Feature `auth-source' reads and writes secrets from files like
;; ~/.netrc for TRAMP and related packages, so for example you can
;; avoid having to type in a particular host's password every time.

(use-feature auth-source
  :demand t
  :config
  (setq auth-sources `(,(no-littering-expand-etc-file-name "authinfo.gpg")
                       ,(expand-file-name "~/.authinfo.gpg")
                       ,(expand-file-name "~/.netrc"))))

;; Package `password-store' integrates Emacs with password-store
;; utility for managing passwords using GnuPG.

(use-package password-store
  :demand t)

;; Package `auth-source-pass' allows `password-store' to be used
;; as `auth-source'.

(use-package auth-source-pass
  :demand t
  :after (auth-source password-store)
  :config
  (let ((dir (password-store-dir)))
    (when (file-directory-p dir)
      (auth-source-pass-enable))))


;;; Selecting behaviors

;; Package `ivy' is an incremental completion and narrowing framework.

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)

  :config
  (ivy-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.

(use-package prescient
  :config
  (prescient-persist-mode +1))

;; Package `ivy-prescient' replaces `ivy' filtering and sorting
;; algorithm with `prescient'.

(use-package ivy-prescient
  :demand t
  :after (ivy prescient)

  :preface
  (eval-when-compile
    (declare-function prescient-persist-mode nil))

  :config
  (ivy-prescient-mode +1))

;; Package `counsel' provides completion UI on top of `ivy'.

(use-package counsel
  :commands
  (ivy-resume
   counsel-switch-buffer
   counsel-switch-buffer-other-window
   counsel-find-file
   counsel-M-x
   counsel-minibuffer-history
   ivy-backward-kill-word
   ivy-alt-done
   ivy-partial-or-done
   ivy-next-line
   ivy-previous-line
   ivy-alt-done
   ivy-previous-line-or-history
   counsel-up-directory)

  :bind
  (("C-c C-r" . #'ivy-resume)
   ("C-x C-b" . #'counsel-switch-buffer)
   ("C-x C-f" . #'counsel-find-file)
   ("M-x"     . #'counsel-M-x)

   :map minibuffer-local-map
   ("C-r"     . #'counsel-minibuffer-history)

   :map ivy-minibuffer-map
   ("<left>"  . #'ivy-backward-kill-word)
   ("<right>" . #'ivy-alt-done)
   ("C-f"     . #'ivy-partial-or-done)
   ("C-h"     . #'ivy-backward-kill-word)
   ("C-j"     . #'ivy-next-line)
   ("C-k"     . #'ivy-previous-line)
   ("C-l"     . #'ivy-alt-done)
   ("C-r"     . #'ivy-previous-line-or-history)

   :map counsel-find-file-map
   ("<left>"  . #'counsel-up-directory)
   ("C-h"     . #'counsel-up-directory))

  :leader
  ("bb"  #'counsel-switch-buffer
   "wbb" #'counsel-switch-buffer-other-window
   "br"  #'counsel-recentf
   "dv"  #'counsel-describe-variable
   "df"  #'counsel-describe-function
   "/"   #'counsel-ag)

  :config
  (setq ivy-initial-inputs-alist nil))

;; Package `ivy-rich' enhances `ivy' and `counsel' completion minibuffer
;; with extra fields for each completion items.

(use-package ivy-rich
  :demand t
  :after ivy

  :init
  (setq ivy-rich-path-style 'abbrev)
  (add-to-list 'ivy-format-functions-alist 'ivy-format-function-line)

  :config
  (ivy-rich-mode +1))

;; Package `swiper' provides a search interface on top of `ivy'.

(use-package swiper
  :commands swiper
  :bind
  (("C-s" . #'swiper)))


;;; Editing behaviors

;; Package `avy' provides an ability to jump to visible text using
;; a char-based decision tree.

(use-package avy
  :commands
  (avy-goto-char
   avy-goto-char-2
   avy-goto-line)

  :leader
  ("jj" #'avy-goto-char
   "jJ" #'avy-goto-char-2
   "jl" #'avy-goto-line))

;; Package `ace-link' utilies `avy' to provide a shortcut to jump to
;; links using a char-based decision tree.

(use-package ace-link
  :commands ace-link

  :leader
  ("jL" #'ace-link))

;; Feature `delsel' provides an alternative behavior for certain
;; actions when you have a selection active. Namely: if you start
;; typing when you have something selected, then the selection will be
;; deleted; and if you press DEL while you have something selected, it
;; will be deleted rather than killed.

(use-feature delsel
  :demand t
  :config
  (delete-selection-mode +1))

;; Feature `warnings' allows us to enable and disable warnings.

(use-feature warnings
  :demand t
  :config
  (add-to-list 'warning-suppress-log-types '(undo discard-info)))

;; Package `undo-tree' replaces the default Emacs undo system, which
;; is poorly designed and hard to use, with a much more powerful
;; tree-based system.

(use-package undo-tree
  :demand t
  :bind
  (:map undo-tree-map
   ("M-/" . #'undo-tree-redo))

  :config
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))

;; Feature `subword' provides a minor mode which causes the
;; `forward-word' and `backward-word' commands to stop at
;; capitalization changes within a word, so that you can step through
;; the components of CamelCase symbols one at a time.

(use-feature subword
  :demand t
  :config
  (global-subword-mode +1))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.

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

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.

(use-package visual-regexp-steroids
  :after visual-regexp
  :init
  (let ((repy (straight--repos-file "visual-regexp-steroids.el/regexp.py")))
    (setq vr/command-python (format "%s %s" "python3" repy))))

;; Package `origami' is a text folding minor mode for Emacs. With this
;; minor mode enabled, you can collapse and expand regions of text.

(use-package origami
  :demand t

  :config
  (global-origami-mode +1)
  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map
      (kbd "TAB")   #'origami-toggle-node
      (kbd "M-TAB") #'origami-toggle-all-nodes)))

;; Package `smartparens' provides an API for manipulating paired
;; delimiters of many different types, as well as interactive commands
;; and keybindings for operating on paired delimiters at the
;; s-expression level. It provides a Paredit compatibility layer.

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
   "sd" #'sp-splice-sexp))

;; Feature `smartparents-config' provides basic configuration for
;; smartparens.

(use-feature smartparens-config
  :after smartparens
  :demand t

  :config
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil))

;; Package `rainbow-delimiters' highlights parens, brackets, and braces
;; according to their depth. Each successive level is highlighted a different
;; color.

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Package `rainbow-mode' highlights text representing color codes in various
;; forms by setting the background color of the text accordingly. This works
;; for color codes in many forms including hexadecimal #RRGGBB color codes,
;; LaTeX {rgb} or {HTML} colors, HTML named and rgb() color, R color names,
;; X color names, and ANSI shell colors.

(use-package rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; Package `dtrt-indent' is a minor mode that guesses the indentation offset
;; originally used for creating source code files and transparently adjusts
;; the corresponding settings in Emacs.

(use-package dtrt-indent
  :commands dtrt-indent-mode)

;; Package `editorconfig' helps maintain consistent coding styles for
;; multiple developers working on the same project across various editors
;; and IDEs.

(use-package editorconfig
  :init

  (defun gemacs--editorconfig-load ()
    "Load `editorconfig' when initially finding a file."
    (require 'editorconfig)
    (remove-hook 'find-file-hook #'gemacs--editorconfig-load))
  (add-hook 'find-file-hook #'gemacs--editorconfig-load)

  :config
  (editorconfig-mode +1))

;; Package `editorconfig-core' provides core functionalities for
;; `editorconfig'.

(use-feature editorconfig-core
  :after editorconfig
  :demand t

  :init
  (defun gemacs--dtrt-maybe-enable ()
    "Enable `dtrt-indent-mode' if `.editorconfig' is not present"
    (when (not (and
		(stringp buffer-file-name)
		(editorconfig-core-get-nearest-editorconfig
		 (file-name-directory buffer-file-name))))
      (dtrt-indent-mode +1)))

  (add-hook 'conf-mode-hook #'gemacs--dtrt-maybe-enable)
  (add-hook 'text-mode-hook #'gemacs--dtrt-maybe-enable)
  (add-hook 'prog-mode-hook #'gemacs--dtrt-maybe-enable))


;;; Projectile

;; Package `projectile' keeps track of a "project" list, which is
;; automatically added to as you visit Git repositories, Node.js
;; projects, etc. It then provides commands for quickly navigating
;; between and within these projects.

(use-package projectile
  :defer 1

  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)

  :config
  (projectile-mode +1)

  (defun gemacs--projectile-invalidate-cache (&rest _args)
    (projectile-invalidate-cache nil))

  :leader
  ("pk" #'projectile-kill-buffers
   "pr" #'projectile-run-project
   "p'" #'projectile-run-eshell
   "p!" #'projectile-run-async-shell-command-in-root))

;; Package `counsel-projectile' provides a `counsel' interface for
;; `projectile' commands.

(use-package counsel-projectile
  :after (counsel projectile)

  :commands
  (counsel-projectile-ag
   counsel-projectile-find-file
   counsel-projectile-switch-project
   counsel-projectile-switch-to-buffer)

  :config
  (counsel-projectile-mode +1)

  :leader
  ("p/" #'counsel-projectile-ag
   "pf" #'counsel-projectile-find-file
   "pp" #'counsel-projectile-switch-project
   "pb" #'counsel-projectile-switch-to-buffer))


;;; Git

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.

(use-package magit
  :commands magit-status

  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  (setq magit-remote-set-if-missing t)

  :config
  (with-eval-after-load 'pinentry
    (advice-add 'magit-start-git :before #'gemacs--gpg-update-tty)
    (advice-add 'magit-call-git :before #'gemacs--gpg-update-tty))

  (with-eval-after-load 'projectile
    (advice-add 'magit-checkout :after #'gemacs--projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout :after #'gemacs--projectile-invalidate-cache))

  :leader
  ("gs" #'magit-status))

;; Feature `git-commit' from package `magit' provides the commit
;; message editing capabilities of Magit.

(use-feature git-commit
  :config
  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.

(use-package forge)

;; Package `ghub' provides a GitHub integration.

(use-feature ghub
  :init

  ;; BUG: https://github.com/magit/ghub/issues/81
  (setq ghub-use-workaround-for-emacs-bug nil))

;; Feature `emacsql-sqlite' is a dependency of Forge which is used to
;; interact with the SQLite database that Forge uses to keep track of
;; information about pull requests.

(use-feature emacsql-sqlite
  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql")))

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.

(use-package git-gutter
  :init

  ;; BUG: https://github.com/syohex/emacs-git-gutter/issues/24
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

  (defun gemacs--git-gutter-load ()
    "Load `git-gutter' when initially finding a file."
    (require 'git-gutter)
    (remove-hook 'find-file-hook #'gemacs--git-gutter-load))
  (add-hook 'find-file-hook #'gemacs--git-gutter-load)

  :config
  (global-git-gutter-mode +1))


;;; Evil

;; Package `evil-mode' provides a VIM-like modal editing experience.

(use-package evil
  :commands (evil-mode evil-define-key turn-on-evil-mode)

  :init
  (setq evil-want-integration +1)
  (setq evil-want-keybinding nil)
  (setq evil-mode-line-format nil)
  (evil-mode +1)

  :config
  (fset 'evil-visual-update-x-selection 'ignore))

;; Package `evil-collection' provides a VIM-like key bindings for
;; several popular packages.

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

;; Package `evil-commentary' provides an easy way to quickly comment
;; a current-selected block or a line.

(use-package evil-commentary
  :after evil
  :demand t
  :config
  (evil-commentary-mode +1))

;; Package `evil-magit' provides a VIM-like keybinding for `magit'.

(use-package evil-magit
  :after (evil magit)
  :demand t)

;; Package `evil-leader' provides a VIM-like leader key invocation
;; e.g. <SPC>bb for opening a list of buffer.

(use-package evil-leader
  :demand t
  :after evil
  :leader
  ("wo" #'other-window
   "wd" #'delete-window
   "wD" #'delete-other-windows
   "w-" #'split-window-below
   "w/" #'split-window-right
   "w=" #'balance-windows
   "bd" #'kill-buffer
   "bD" #'kill-buffer-and-window)

  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode +1))

;; Package `evil-matchit' provides a shortcut to jump between matching
;; surrounding pairs (e.g. parenthesis).

(use-package evil-matchit
  :demand t
  :after evil
  :config
  (global-evil-matchit-mode +1))

;; Package `evil-surround' provides an ability to manipulate matching
;; surrounding pairs (e.g. parenthesis).

(use-package evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode +1))


;;; Treemacs

;; Package `treemacs' is a file and project explorer similar to
;; package `neotree' or vim's NerdTree. It shows the file system outlines
;; of your projects in a simple tree layout.

(use-package treemacs
  :commands
  (treemacs
   treemacs-switch-workspace
   treemacs-add-project-to-workspace
   treemacs-create-workspace
   treemacs-remove-project-from-workspace
   treemacs-remove-workspace)

  :leader
  ("tt" #'treemacs
   "ts" #'treemacs-switch-workspace
   "tc" #'treemacs-add-project-to-workspace
   "tC" #'treemacs-create-workspace
   "td" #'treemacs-remove-project-from-workspace
   "tD" #'treemacs-remove-workspace))

;; Package `treemacs-projectile' integrates `treemacs' with `projectile'.

(use-package treemacs-projectile
  :after (treemacs projectile)
  :demand t
  :leader
  ("tp" #'treemacs-projectile))

;; Package `treemacs-magit' integrates `treemacs' with `magit'.

(use-package treemacs-magit
  :after (treemacs magit)
  :demand t)

;; Package `treemacs-magit' integrates `treemacs' with `evil'.

(use-package treemacs-evil
  :after (treemacs evil)
  :demand t

  :preface
  (eval-when-compile
    (declare-function treemacs-TAB-action nil)
    (declare-function treemacs-RET-action nil))

  :bind
  (:map evil-treemacs-state-map
   ([return] . #'treemacs-RET-action)
   ([tab]    . #'treemacs-TAB-action)
   ("TAB"    . #'treemacs-TAB-action)))


;;; Frame and window behaviors

;; Setup a minimalist frame without toolbar and menu bar, but also taking
;; OS behavior quirks into consideration. This is done via `make-frame-func'
;; to allow GUI `emacsclient' connecting to `emacs-server' to have different
;; frame settings independent of CLI ones.

(defvar gemacs-font "PragmataPro Mono")
(defvar gemacs-font-size 11)

(eval-when-compile
  (declare-function scroll-bar-mode nil)
  (declare-function mac-auto-operator-composition-mode nil))

(setq-default frame-title-format '("%f"))
(tool-bar-mode -1)
(menu-bar-mode -1)

(defun gemacs--after-make-frame-func (frame)
  "Setup frame attributes after a FRAME is created."
  (if (display-graphic-p frame)
      (let ((w (window-system frame)))
        (cond
	 ((eq w 'x)
	  (scroll-bar-mode -1)
	  (set-frame-font (format "%s %s" gemacs-font gemacs-font-size)
			  nil
			  t))
	 ((eq w 'mac)
	  (progn
	    ;; macOS will "float" Emacs window if menu-bar-mode is disabled.
	    ;; (e.g. not sticky to Spaces and no fullscreen support)
	    (menu-bar-mode 1)
	    (scroll-bar-mode -1)
	    ;; macOS display font size about x1.2 smaller than other Unices.
	    (set-frame-font (format "%s %s"
				    gemacs-font
				    (round (* gemacs-font-size 1.2)))
			    nil
			    t)
	    (when (boundp 'mac-auto-operator-composition-mode)
	      (mac-auto-operator-composition-mode))))))

    (progn
      (eval-and-compile
        (defun gemacs-scroll-down ()
          "Scroll down three lines."
          (interactive)
          (scroll-down 3))

        (defun gemacs-scroll-up ()
          "Scroll up three lines."
          (interactive)
          (scroll-up 3)))

      (xterm-mouse-mode t)
      (bind-key "<mouse-4>" #'gemacs-scroll-down)
      (bind-key "<mouse-5>" #'gemacs-scroll-up))))

(add-hook 'after-make-frame-functions #'gemacs--after-make-frame-func)
(add-hook 'after-init-hook
          `(lambda ()
             (gemacs--after-make-frame-func (selected-frame))))

;; Package `winum' provides Emacs window navigation and frames using
;; numbers. It is an extended and actively maintained version of the
;; `window-numbering' package with some ideas and code taken from
;; `ace-window'.

(use-package winum
  :demand t
  :preface
  (eval-when-compile
    (declare-function treemacs-current-visibility nil)
    (declare-function treemacs-get-local-window nil)
    (declare-function treemacs-get-local-buffer nil))

  :init
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local)

  :config
  (winum-mode +1)

  (defun gemacs--winum-select-window-0-or-treemacs ()
    "Select either window 0 or treemacs window if exists."
    (interactive)
    (require 'treemacs)
    (cond
     ((eq (treemacs-current-visibility) 'visible) (treemacs-select-window))
     (t (winum-select-window-0))))

  :leader
  ("0" #'gemacs--winum-select-window-0-or-treemacs
   "1" #'winum-select-window-1
   "2" #'winum-select-window-2
   "3" #'winum-select-window-3
   "4" #'winum-select-window-4
   "5" #'winum-select-window-5
   "6" #'winum-select-window-6
   "7" #'winum-select-window-7
   "8" #'winum-select-window-8
   "9" #'winum-select-window-9))


;;; Snippets

;; Feature `abbrev' provides functionality for expanding user-defined
;; abbreviations. We prefer to use `yasnippet' instead, though.

(use-feature abbrev)

;; Package `yasnippet' allows the expansion of user-defined
;; abbreviations into fillable templates. The only reason we have it
;; here is because it gets pulled in by LSP, and we need to unbreak
;; some stuff.

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
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.
  (use-feature company
    :config

    (defun radian--yasnippet-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
          event
	(vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (defvar radian--yasnippet-then-company-keymap
      ;; It starts out as a copy of `yas-keymap', and then we
      ;; merge in all of the bindings from `company-active-map'.
      (let ((keymap (copy-keymap yas-keymap)))
	(map-keymap
	 (lambda (event company-cmd)
	   (let* ((event (radian--yasnippet-normalize-event event))
		  (yas-cmd (lookup-key yas-keymap event)))
	     ;; Here we use an extended menu item with the
	     ;; `:filter' option, which allows us to dynamically
	     ;; decide which command we want to run when a key is
	     ;; pressed.
	     (define-key keymap event
	       `(menu-item
		 nil ,company-cmd :filter
		 (lambda (cmd)
		   ;; There doesn't seem to be any obvious
		   ;; function from Company to tell whether or not
		   ;; a completion is in progress (à la
		   ;; `company-explicit-action-p'), so I just
		   ;; check whether or not `company-my-keymap' is
		   ;; defined, which seems to be good enough.
		   (if company-my-keymap
		       ',company-cmd
		     ',yas-cmd))))))
	 company-active-map)
	keymap)
      "Keymap which delegates to both `company-active-map' and `yas-keymap'.
The bindings in `company-active-map' only apply if Company is
currently active.")

    (radian-defadvice radian--advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      :around #'yas--make-control-overlay
      "Allow `company' keybindings to override those of `yasnippet'."
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (let ((yas-keymap radian--yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args)))))


;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code.

(defvar-local radian--company-buffer-modified-counter nil
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
  (setq company-show-numbers t)
  (setq company-require-match #'company-explicit-action-p)
  (setq company-auto-complete-chars nil)
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t)

  (radian-defadvice radian--advice-company-complete-on-change ()
    :override #'company--should-begin
    "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick radian--company-buffer-modified-counter)
        ;; Only trigger completion if previous counter value was
        ;; non-nil (i.e., don't trigger completion just as we're
        ;; jumping to a buffer for the first time).
        (prog1 radian--company-buffer-modified-counter
          (setq radian--company-buffer-modified-counter tick)))))

  (global-company-mode +1))

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.

(use-package company-prescient
  :demand t
  :after (company prescient)
  :config
  (company-prescient-mode +1))

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.


;;; Autoformatting

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :commands apheleia-global-mode

  :init
  (defun gemacs--apheleia-load ()
    "Load `apheleia' when initially finding a file."
    (require 'apheleia)
    (apheleia-global-mode +1)
    (remove-hook 'find-file-hook #'gemacs--apheleia-load))

  (radian-defadvice radian--save-buffer-reformat-maybe (func &optional arg)
    :around #'save-buffer
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  (add-hook 'find-file-hook #'gemacs--apheleia-load)

  :config
  (add-to-list 'apheleia-formatters '(goimports . ("goimports")))
  (add-to-list 'apheleia-formatters '(jsonnetfmt . ("jsonnetfmt"
                                                     "--indent" "2"
                                                     "--max-blank-lines" "2"
                                                     "--sort-imports"
                                                     "--string-style" "s"
                                                     "--comment-style" "s"
                                                     "--" "-")))

  (add-to-list 'apheleia-mode-alist '(go-mode . goimports))
  (add-to-list 'apheleia-mode-alist '(jsonnet-mode . jsonnetfmt))
  (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier)))


;;; Errors and documentation

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting, or more generally syntax checking. It comes
;; with a large number of checkers pre-defined, and other packages
;; define more.

(use-package flycheck
  :defer 4
  :commands (flycheck-list-errors)

  :preface
  (eval-when-compile
    (declare-function flycheck-previous-error nil)
    (declare-function flycheck-next-error nil)
    (declare-function flycheck-list-errors nil)
    (declare-function flycheck-overlay-errors-at nil)
    (declare-function flycheck-error-line-region nil))

  :init
  (defun radian--flycheck-disable-checkers (&rest checkers)
    "Disable the given Flycheck syntax CHECKERS, symbols.
This function affects only the current buffer, and neither causes
nor requires Flycheck to be loaded."
    (unless (boundp 'flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers nil))
    (make-local-variable 'flycheck-disabled-checkers)
    (dolist (checker checkers)
      (cl-pushnew checker flycheck-disabled-checkers)))

  :bind-keymap (("C-c !" . #'flycheck-command-map))
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
  (setq flycheck-display-errors-delay 0.2)

  :leader
  ("fp" #'flycheck-previous-error
   "fn" #'flycheck-next-error
   "fl" #'flycheck-list-errors))

;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.

(use-feature eldoc
  :demand t
  :config

  (radian-when-compiletime (version<= "27" emacs-version)
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

  (radian-when-compiletime (and (version< emacs-version "27")
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

  (radian-when-compiletime (version< emacs-version "26")
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

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  (use-feature flycheck
    :config
    (radian-defadvice radian--advice-disable-eldoc-on-flycheck
        (&rest _)
      :after-while #'eldoc-display-message-no-interference-p
      "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
      (not (flycheck-overlay-errors-at (point)))))

  (radian-defadvice radian--advice-eldoc-better-display-message-p (&rest _)
    :override #'eldoc--message-command-p
    "Make ElDoc smarter about when to display its messages.
By default ElDoc has a customizable whitelist of commands that it
will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't."
    (member (current-message) (list nil eldoc-last-message))))


;;; Language Server Protocol

;; Package `lsp-mode' is an Emacs client for the Language Server
;; Protocol <https://langserver.org/>. It is where we get all of our
;; information for completions, definition location, documentation,
;; and so on.

(use-package lsp-mode
  :preface
  (eval-when-compile
    (declare-function lsp-resolve-final-function nil))

  :init
  (radian-defhook radian--lsp-enable ()
    after-change-major-mode-hook
    "Enable `lsp-mode' for most programming modes.
Do this on `after-change-major-mode-hook' instead of
`prog-mode-hook' and `text-mode-hook' because we want to make
sure regular mode hooks get a chance to run first, for example to
set LSP configuration (see `lsp-python-ms')."
    (when (derived-mode-p #'prog-mode #'text-mode)
      (unless (or (null buffer-file-name)
                  (derived-mode-p
                   ;; `lsp-mode' doesn't support Elisp, so let's avoid
                   ;; triggering the autoload just for checking that, yes,
                   ;; there's nothing to do for the *scratch* buffer.
                   #'emacs-lisp-mode
                   ;; Disable for modes that we currently use a specialized
                   ;; framework for, until they are phased out in favor of
                   ;; LSP.
                   ;;#'clojure-mode
                   #'ruby-mode
                   #'rust-mode))
        (lsp))))

  :config
  (defun radian--advice-lsp-mode-silence (format &rest args)
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

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'radian--advice-lsp-mode-silence))

  ;; If we don't disable this, we get a warning about YASnippet not
  ;; being available, even though it is. I don't use YASnippet anyway,
  ;; so don't bother with it.
  (setq lsp-enable-snippet nil)

  (radian-defadvice radian--lsp-run-from-node-modules (command)
    :filter-return #'lsp-resolve-final-function
    "Find LSP executables inside node_modules/.bin if present."
    (cl-block nil
      (prog1 command
        (when-let ((project-dir
                    (locate-dominating-file default-directory "node_modules"))
                   (binary
                    (radian--path-join
                     project-dir "node_modules" ".bin" (car command))))
          (when (file-executable-p binary)
            (cl-return (cons binary (cdr command))))))))

  (radian-defhook radian--lsp-teardown ()
    kill-emacs-hook
    "Ignore the LSP server getting killed.
If we don't do this, then when killing Emacs we may be prompted
with whether we want to restart the LSP server that has just been
killed (which happens during Emacs shutdown)."
    (setq lsp-restart nil))

  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  (setq lsp-language-id-configuration
        (mapcar
         (lambda (link)
           (if (and (stringp (car link))
                    (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
               (cons
                (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
             link))
         lsp-language-id-configuration)))

;; Package `company-lsp' provides a Company backend for `lsp-mode'.
;; It's configured automatically by `lsp-mode'.

(use-package company-lsp
  :init

  (use-feature lsp
    :config

    (radian-defadvice radian--company-lsp-setup (&rest _)
      :after #'lsp
      "Disable `company-prescient' sorting by length in some contexts.
Specifically, disable sorting by length if the LSP Company
backend returns fuzzy-matched candidates, which implies that the
backend has already sorted the candidates into a reasonable
order."
      (setq-local company-prescient-sort-length-enable
                  (cl-dolist (w lsp--buffer-workspaces)
                    (when (thread-first w
                            (lsp--workspace-client)
                            (lsp--client-server-id)
                            (memq '(jsts-ls mspyls bash-ls texlab ts-ls))
                            (not))
                      (cl-return t)))))))

;; Package `lsp-ui' provides Flycheck integration for `lsp-mode', as
;; well as various other UI elements that integrate with `lsp-mode'.
;; It's configured automatically by `lsp-mode'.

(use-package lsp-ui
  :commands lsp-ui-sideline-apply-code-actions
  :bind (("C-c f" . #'lsp-ui-sideline-apply-code-actions))

  :config
  (radian-defadvice radian--advice-lsp-ui-apply-single-fix
      (orig-fun &rest args)
    :around #'lsp-ui-sideline-apply-code-actions
    "Apply code fix immediately if only one is possible."
    (radian-flet ((defun completing-read (prompt collection &rest args)
                    (if (= (safe-length collection) 1)
                        (car collection)
                      (apply completing-read prompt collection args))))
      (apply orig-fun args)))

  ;; Don't show symbol definitions in the sideline. They are pretty
  ;; noisy, and there appears to currently be a bug where they prevent
  ;; Flycheck errors from being shown (the errors flash briefly and
  ;; then disappear).
  (setq lsp-ui-sideline-show-hover nil)

  (use-feature lsp-mode
    :config

    ;; With `lsp-ui', there's no need for the ElDoc integration
    ;; provided by `lsp-mode', and in fact for Bash it is very
    ;; annoying since all the hover information is multiline.
    (setq lsp-eldoc-enable-hover nil)))

;; Feature `lsp-ui-doc' from package `lsp-ui' displays documentation
;; in a child frame when point is on a symbol.

(use-feature lsp-ui-doc
  :preface
  (eval-when-compile
    (declare-function lsp-ui-doc--render-buffer nil))

  :config

  ;; https://github.com/emacs-lsp/lsp-ui/issues/414
  (setq lsp-ui-doc-use-childframe nil)

  (radian-defadvice radian--advice-lsp-ui-doc-allow-multiline (func &rest args)
    :around #'lsp-ui-doc--render-buffer
    "Prevent `lsp-ui-doc' from removing newlines from documentation."
    (radian-flet ((defun replace-regexp-in-string
                      (regexp rep string &rest args)
                    (if (equal regexp "`\\([\n]+\\)")
                        string
                      (apply replace-regexp-in-string
                             regexp rep string args))))
      (apply func args))))


;;; Language support

;;;; Text-based languages

;; Feature `text-mode' provides a major mode for editing plain text.

(use-feature text-mode
  :config
  (radian-defhook radian--flycheck-text-setup ()
    text-mode-hook
    "Disable some Flycheck checkers for plain text."
    (radian--flycheck-disable-checkers 'proselint)))


;;;; Lisp languages

;; Feature `lisp-mode' provides a base major mode for Lisp languages,
;; and supporting functions for dealing with Lisp code.
(use-feature lisp-mode
  :init
  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))


;;;; Emacs Lisp

;; Package `helpful' provides a complete replacement for the built-in
;; Emacs help facility which provides much more contextual information
;; in a better format.

(use-package helpful
  :commands
  (helpful-callable
   helpful-variable
   helpful-symbol
   helpful-key
   helpful-function
   helpful-macro
   helpful-command
   helpful-at-point)

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
   ("C-c C-d" . #'helpful-at-point))

  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

;; Feature `elisp-mode' provides the major mode for Emacs Lisp. Very
;; important! It also provides the major mode for the *scratch*
;; buffer, which is very similar but slightly different. Not as
;; important.

(use-feature elisp-mode
  :config
  (radian-defhook radian--flycheck-elisp-setup ()
    emacs-lisp-mode-hook
    "Disable some Flycheck checkers for Emacs Lisp."
    ;; These checkers suck at reporting error locations, so they're
    ;; actually quite distracting to work with.
    (radian--flycheck-disable-checkers 'emacs-lisp 'emacs-lisp-checkdoc))

  ;; Note that this function is actually defined in `elisp-mode'
  ;; because screw modularity.
  (radian-defadvice radian--advice-company-elisp-use-helpful
      (func &rest args)
    :around #'elisp--company-doc-buffer
    "Cause `company' to use Helpful to show Elisp documentation."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable)
              ((symbol-function #'help-buffer) #'current-buffer))
      (apply func args)))

  (radian-defadvice radian--advice-fill-elisp-docstrings-correctly (&rest _)
    :before-until #'fill-context-prefix
    "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
    (when (and (derived-mode-p #'emacs-lisp-mode)
               (eq (get-text-property (point) 'face) 'font-lock-doc-face))
      "")))


;;;; Go
;; https://golang.org/

;; Package `go-mode' provides a major mode for Go.

(use-package go-mode
  :config

  (eval-when-compile
    (require 'go-mode))

  (defvar radian--go-defun-regexp
    "^\\(const\\|func\\|import\\|interface\\|package\\|type\\|var\\)"
    "Regexp matching top-level declarations in Go.")

  (defun radian--go-beginning-of-defun (&optional arg)
    "Move to beginning of current or previous top-level declaration."
    (cond
     ((null arg)
      (cl-block nil
        (while t
          (re-search-backward radian--go-defun-regexp nil 'noerror)
          (when (or (bobp)
                    (eq (get-text-property (point) 'face)
                        'font-lock-keyword-face))
            (cl-return)))))
     ((> arg 0)
      (dotimes (_ arg)
        (radian--go-beginning-of-defun)))
     ((< arg 0)
      ;; Yuck -- but we need to implement this, otherwise
      ;; `end-of-defun' just does the wrong thing :/
      (dotimes (_ (- arg))
        (radian--go-beginning-of-defun)
        (radian--go-end-of-defun)
        (radian--go-end-of-defun))
      (radian--go-beginning-of-defun))))

  (defun radian--go-end-of-defun ()
    "Move to end of current or previous top-level declaration.
Only works if `radian--go-beginning-of-defun' was just called
previously."
    (dotimes (_ 2)
      (cl-block nil
        (while t
          (re-search-forward radian--go-defun-regexp nil 'noerror)
          (when (or (eobp)
                    (save-excursion
                      (beginning-of-line)
                      (eq (get-text-property (point) 'face)
                          'font-lock-keyword-face)))
            (cl-return)))))
    (beginning-of-line)
    (go--backward-irrelevant 'stop-at-string)
    (forward-line))

  (defun radian--go-defun-setup ()
    "Set up \\[beginning-of-defun] and \\[end-of-defun] correctly.
See <https://github.com/dominikh/go-mode.el/issues/232>."
    (setq-local beginning-of-defun-function #'radian--go-beginning-of-defun)
    (setq-local end-of-defun-function #'radian--go-end-of-defun))

  (add-hook 'go-mode-hook #'radian--go-defun-setup)

  (use-feature lsp-ui
    :preface
    (eval-when-compile
      (declare-function lsp-ui-sideline--code-actions nil))

    :config
    (radian-defadvice radian--advice-lsp-ui-organize-imports-more-cleanly
        (func actions &rest args)
      :around #'lsp-ui-sideline--code-actions
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
        (setq lsp-ui-sideline--code-actions actions-to-keep)))))


;;;; Python
;; https://www.python.org/

;; Feature `python' provides a major mode for Python.

(use-feature python
  :preface
  (eval-when-compile
    (defvar lsp-python-executable-cmd))

  :config

  ;; The only consistent style.
  (setq python-fill-docstring-style 'django)

  (radian-defhook radian--python-fix-outline-mode-config ()
    python-mode-hook
    "Prevent `python-mode' from overriding `outline-minor-mode' config.
If this hook is not used, then `python-mode' will override even a
file-local setting of e.g. `outline-regexp' with its own setting."
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp))

  (radian-defhook radian--python-no-reindent-on-colon ()
    python-mode-hook
    "Don't reindent on typing a colon.
See https://emacs.stackexchange.com/a/3338/12534."
    (setq electric-indent-chars (delq ?: electric-indent-chars)))

  ;; Default to Python 3. Prefer the versioned Python binaries since
  ;; some systems stupidly make the unversioned one point at Python 2.
  (cond
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  (radian-defhook radian--python-use-correct-executables ()
    python-mode-hook
    "Use the correct Python executables for tooling."
    (let ((executable python-shell-interpreter))
      (save-excursion
        (save-match-data
          (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                    (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
            (setq executable (substring-no-properties (match-string 1))))))
      ;; Try to compile using the appropriate version of Python for
      ;; the file.
      (setq-local flycheck-python-pycompile-executable executable)
      ;; We might be running inside a virtualenv, in which case the
      ;; modules won't be available. But calling the executables
      ;; directly will work.
      (setq-local flycheck-python-pylint-executable "pylint")
      (setq-local flycheck-python-flake8-executable "flake8")
      ;; Use the correct executable for the language server.
      (setq-local lsp-python-executable-cmd executable)))

  ;; I honestly don't understand why people like their packages to
  ;; spew so many messages.
  (setq python-indent-guess-indent-offset-verbose nil)

  (defun radian--python-find-virtualenv ()
    "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
    (cl-block nil
      (when (and (executable-find "poetry")
                 (locate-dominating-file default-directory "pyproject.toml"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process
                      "poetry" nil '(t nil) nil "run" "which" "python"))
            (goto-char (point-min))
            (when (looking-at "\\(.+\\)/bin/python\n")
              (let ((venv (match-string 1)))
                (when (file-directory-p venv)
                  (cl-return venv)))))))
      (when (and (executable-find "pipenv")
                 (locate-dominating-file default-directory "Pipfile"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process "pipenv" nil '(t nil) nil "--venv"))
            (goto-char (point-min))
            (let ((venv (string-trim (buffer-string))))
              (when (file-directory-p venv)
                (cl-return venv))))))))

  (radian-defhook radian--flycheck-python-setup ()
    python-mode-hook
    "Disable some Flycheck checkers for Python."
    (radian--flycheck-disable-checkers 'python-flake8)))

;; Package `lsp-python-ms' downloads Microsoft's LSP server for Python
;; and configures it with `lsp-mode'. Microsoft's server behaves
;; better than Palantir's in my opinion.

(use-package lsp-python-ms
  :demand t
  :after (lsp-clients python)

  :preface
  (eval-when-compile
    (declare-function lsp-python-ms--language-server-started-callback nil)
    (declare-function lsp-python-ms--extra-init-params nil))

  :config
  (radian-defadvice radian--lsp-python-ms-silence (func &rest args)
    :around #'lsp-python-ms--language-server-started-callback
    "Inhibit a silly message."
    (radian--with-silent-message "Python language server started"
      (apply func args)))

  (radian-defadvice radian--lsp-python-ms-discover-virtualenvs
      (func &rest args)
    :around #'lsp-python-ms--extra-init-params
    "Automatically discover Pipenv and Poetry virtualenvs."
    (let ((lsp-python-ms-extra-paths lsp-python-ms-extra-paths)
          (exec-path exec-path))
      (when-let ((venv (radian--python-find-virtualenv)))
        (setq lsp-python-ms-extra-paths
              (file-expand-wildcards
               (expand-file-name
                "lib/python*/site-packages" venv)))
        (push (expand-file-name "bin" venv) exec-path))
      (apply func args))))

(use-package jinja2-mode
  :mode ("\\.j2\\'" "\\.jinja2\\'"))


;;;; ReST
;; http://docutils.sourceforge.net/rst.html

;; Feature `rst' provides a major mode for ReST.
(use-feature rst
  :config

  (radian-defhook radian--flycheck-rst-setup ()
    rst-mode-hook
    "If inside Sphinx project, disable the `rst' Flycheck checker.
This prevents it from signalling spurious errors. See also
https://github.com/flycheck/flycheck/issues/953."
    (when (locate-dominating-file default-directory "conf.py")
      (radian--flycheck-disable-checkers 'rst))))


;;;; Ruby
;; https://www.ruby-lang.org/

;; Package `robe' provides a language server for Ruby which draws
;; information for autocompletions and source code navigation from a
;; live REPL in the project context. Start it with `robe-start'.

(use-package robe
  :init
  (add-hook 'ruby-mode-hook #'robe-mode))

;; Package `ruby-electric' allows you to have Emacs insert a paired
;; "end" when you type "do", and analogously for other paired
;; keywords.

(use-package ruby-electric
  :init/el-patch

  ;; We already have paired delimiter support from Smartparens.
  ;; However, `ruby-electric' provides its own copy of this
  ;; functionality, in a less optimal way. (In particular, typing a
  ;; closing paren when your cursor is right before a closing paren
  ;; will insert another paren rather than moving through the existing
  ;; one.) Unfortunately, `ruby-electric-delimiters-alist' is defined
  ;; as a constant, so we can't customize it by setting it to nil
  ;; (actually, we can, but byte-compilation inserts the value
  ;; literally at its use sites, so this does not take effect).
  ;; Instead, we override the definition of `ruby-electric-mode-map'
  ;; to make it ignore `ruby-electric-delimiters-alist'. Also note
  ;; that we are actually doing this before `ruby-electric' is loaded.
  ;; This is so that the modification will actually affect the
  ;; definition of `ruby-electric-mode', which gets whatever value
  ;; `ruby-electric-mode-map' happens to have at definition time. (The
  ;; alternative is to also patch `ruby-electric-mode'.)

  (defvar ruby-electric-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map " " 'ruby-electric-space/return)
      (define-key
        map [remap delete-backward-char] 'ruby-electric-delete-backward-char)
      (define-key map [remap newline] 'ruby-electric-space/return)
      (define-key map [remap newline-and-indent] 'ruby-electric-space/return)
      (define-key
        map [remap electric-newline-and-maybe-indent]
        'ruby-electric-space/return)
      (define-key
        map [remap reindent-then-newline-and-indent]
        'ruby-electric-space/return)
      (el-patch-remove
        (dolist (x ruby-electric-delimiters-alist)
          (let* ((delim   (car x))
                 (plist   (cdr x))
                 (name    (plist-get plist :name))
                 (func    (plist-get plist :handler))
                 (closing (plist-get plist :closing)))
            (define-key map (char-to-string delim) func)
            (if closing
                (define-key
                  map (char-to-string closing) 'ruby-electric-closing-char)))))
      map)
    (el-patch-concat
      "Keymap used in ruby-electric-mode"
      (el-patch-add ".\n\nThe single-character bindings have been removed.")))

  :init
  (add-hook 'ruby-mode #'ruby-electric-mode))


;;;; Rust
;; https://www.rust-lang.org/

;; Package `rust-mode' provides a major mode for Rust.

(use-package rust-mode)

;; Package `racer' provides a language server for Rust, and a Company
;; backend which uses this server to display autocompletions. Racer
;; also provides source code navigation support.

(use-package racer
  :init
  (add-hook 'rust-mode #'racer-mode))


;;;; Shell
;; http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
;; https://www.gnu.org/software/bash/
;; http://www.zsh.org/

(use-feature sh-script
  :config
  (dolist (func '(sh-set-shell sh-make-vars-local))
    (advice-add func :around #'radian--advice-silence-messages))

  (radian-defhook radian--sh-prettify-mode-line ()
    sh-mode-hook
    "Instead of \"Shell[bash]\", display mode name as \"Bash\"."
    ;; Only do this for `sh-mode', not derived modes such as
    ;; `pkgbuild-mode'.
    (setq mode-line-process nil)
    (when (eq major-mode 'sh-mode)
      (setq mode-name (capitalize (symbol-name sh-shell)))))

  (use-feature lsp-clients
    :config

    ;; Only activate the Bash LSP server in Bash code, not all shell
    ;; script code. It's not very helpful to get Bash syntax errors
    ;; while editing Zsh code.
    (radian-protect-macros
      (setf (lsp--client-activation-fn (gethash 'bash-ls lsp-clients))
            (lambda (&rest _)
              (memq sh-shell '(sh bash)))))))


;;;; Web
;; https://developer.mozilla.org/en-US/docs/web/HTML
;; https://developer.mozilla.org/en-US/docs/Web/CSS
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript

;; Feature `js' provides a major mode `js-mode' for JavaScript. We
;; don't use it (because `web-mode' is better), but we still configure
;; some of its variables because `json-mode' uses them.

(use-feature js
  :config
  ;; Default is 4, and nobody should indent JSON with four spaces.
  (setq js-indent-level 2))

;; Package `web-mode' provides a major mode for HTML, CSS, JavaScript,
;; and every conceivable thing adjacent (TypeScript, JSX, TSX, PSP,
;; ASP, Handlebars, etc.) all at once.

(use-package web-mode
  ;; Unfortunately `web-mode' does not come with `auto-mode-alist'
  ;; autoloads. We have to establish them manually. This list comes
  ;; from the official website at <http://web-mode.org/> as of
  ;; 2018-07-09.
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.jsx?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.css\\'" . web-mode))

  ;; Use `web-mode' rather than `js-mode' for scripts.
  :interpreter (("js" . web-mode)
                ("node" . web-mode))

  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-auto-quoting nil)

  ;; When using `web-mode' to edit JavaScript files, support JSX tags.
  (add-to-list 'web-mode-content-types-alist
               '("jsx" . "\\.js[x]?\\'"))

  ;; Create line comments instead of block comments by default in
  ;; JavaScript. See <https://github.com/fxbois/web-mode/issues/619>.
  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
          (cl-remove-if (lambda (item)
                          (member (car item) types))
                        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats)))

  (radian-defhook radian--web-js-fix-comments ()
    web-mode-hook
    "Fix comment handling in `web-mode' for JavaScript."
    (when (member web-mode-content-type '("javascript" "jsx"))

      ;; For some reason the default is to insert HTML comments even
      ;; in JavaScript.
      (setq-local comment-start "//")
      (setq-local comment-end "")

      ;; Needed since otherwise the default value generated by
      ;; `comment-normalize-vars' will key off the syntax and think
      ;; that a single "/" starts a comment, which completely borks
      ;; auto-fill.
      (setq-local comment-start-skip "// *")))

  (use-feature apheleia
    :config

    (radian-defhook radian--web-highlight-after-formatting ()
      apheleia-post-format-hook
      "Make sure syntax highlighting works with Apheleia.
The problem is that `web-mode' doesn't do highlighting correctly
in the face of arbitrary buffer modifications, and kind of hacks
around the problem by hardcoding a special case for yanking based
on the value of `this-command'. So, when buffer modifications
happen in an unexpected (to `web-mode') way, we have to manually
poke it. Otherwise the modified text remains unfontified."
      (setq web-mode-fontification-off nil)
      (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
        (save-excursion
          (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end))))))

;;;; Makefile

;; Feature `make-mode' provides major modes for editing Makefiles.
(use-feature make-mode)


;;;; Markdown
;; https://daringfireball.net/projects/markdown/

;; Package `markdown-mode' provides a major mode for Markdown.
(use-package markdown-mode
  :mode (;; Extension used by Hugo.
         ("\\.mmark\\'" . markdown-mode))

  :preface
  (eval-when-compile
    (declare-function markdown-table-at-point-p nil)
    (declare-function markdown-table-forward-cell nil)
    (declare-function markdown-list-item-at-point-p nil)
    (declare-function markdown-demote-list-item nil)
    (declare-function markdown-table-backward-cell nil)
    (declare-function markdown-promote-list-item nil)
    (declare-function markdown-match-generic-metadata nil)
    (declare-function markdown-insert-pre nil)
    (declare-function markdown-insert-blockquote nil))

  :bind (;; C-c C-s p is a really dumb binding, we prefer C-c C-s C-p.
         ;; Same for C-c C-s q.
         :map markdown-mode-style-map
         ("C-p" . #'markdown-insert-pre)
         ("C-q" . #'markdown-insert-blockquote)
         :map markdown-mode-map
         ("TAB" . #'radian-markdown-tab)
         ;; Try to override all the bindings in
         ;; `markdown-mode-map'...
         ("<S-iso-lefttab>" . #'radian-markdown-shifttab)
         ("<S-tab>" . #'radian-markdown-shifttab)
         ("<backtab>" . #'radian-markdown-shifttab))

  :config
  (eval-and-compile
    (defun radian-markdown-tab ()
      "Do something reasonable when the user presses TAB.
This means moving forward a table cell, indenting a list item, or
performing normal indentation."
      (interactive)
      (cond
        ((markdown-table-at-point-p) (markdown-table-forward-cell))
        ((markdown-list-item-at-point-p) (markdown-demote-list-item))
        (t
          ;; Ew. But `markdown-indent-line' checks to see if
          ;; `this-command' is `markdown-cycle' before doing something
          ;; useful, so we have to.
          (let ((this-command 'markdown-cycle))
            (indent-for-tab-command)))))

    (defun radian-markdown-shifttab ()
      "Do something reasonable when the user presses S-TAB.
This means moving backward a table cell or unindenting a list
item."
      (interactive)
      (cond
        ((markdown-table-at-point-p) (markdown-table-backward-cell))
        ((markdown-list-item-at-point-p) (markdown-promote-list-item)))))

  (radian-defhook radian--flycheck-markdown-setup ()
    markdown-mode-hook
    "Disable some Flycheck checkers for Markdown."
    (radian--flycheck-disable-checkers
     'markdown-markdownlint-cli
     'markdown-mdl
     'proselint))

  (radian-defadvice radian--disable-markdown-metadata-fontification (&rest _)
    :override #'markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
https://github.com/jrblevin/markdown-mode/issues/328."
    (prog1 nil (goto-char (point-max)))))


;;; File formats

;; Package `apache-mode' provides a major mode for .htaccess and
;; similar files.

(use-package apache-mode)

;; Package `crontab-mode' provides a major mode for crontab files.

(use-package crontab-mode)

;; Package `dockerfile-mode' provides a major mode for Dockerfiles.

(use-package dockerfile-mode)

;; Package `gitconfig-mode' provides a major mode for .gitconfig and
;; .gitmodules files.

(use-package gitconfig-mode)

;; Package `gitignore-mode' provides a major mode for .gitignore
;; files.

(use-package gitignore-mode)

;; Package `json-mode' provides a major mode for JSON.

(use-package json-mode
  :init/el-patch
  (defconst json-mode-standard-file-ext '(".json" ".jsonld")
    "List of JSON file extensions.")

  (defsubst json-mode--update-auto-mode (filenames)
    "Update the `json-mode' entry of `auto-mode-alist'.
FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
    (let* ((new-regexp
            (rx-to-string
             `(seq (eval
                    (cons 'or
                          (append json-mode-standard-file-ext
                                  ',filenames)))
                   eot)))
           (new-entry (cons new-regexp 'json-mode))
           (old-entry (when (boundp 'json-mode--auto-mode-entry)
                        json-mode--auto-mode-entry)))
      (setq auto-mode-alist (delete old-entry auto-mode-alist))
      (add-to-list 'auto-mode-alist new-entry)
      new-entry))

  (defcustom json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock")
    "List of filename as string to pass for the JSON entry of
`auto-mode-alist'.
Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected."
    :group 'json-mode
    :type '(repeat string)
    :set (lambda (symbol value)
           "Update SYMBOL with a new regexp made from VALUE.
This function calls `json-mode--update-auto-mode' to change the
`json-mode--auto-mode-entry' entry in `auto-mode-alist'."
           (set-default symbol value)
           (setq json-mode--auto-mode-entry
                 (json-mode--update-auto-mode value))))

  (defvar json-mode--auto-mode-entry
    (json-mode--update-auto-mode json-mode-auto-mode-list)
    "Regexp generated from the `json-mode-auto-mode-list'.")

  :config
  (radian-defhook radian--fix-json-indentation ()
    json-mode-hook
    "Set the tab width to 2 for JSON."
    (setq-local tab-width 2))

  (use-feature flycheck
    :config
    ;; Handle an error message that occurs when the buffer has only
    ;; whitespace, and in some other circumstances, which for some
    ;; bizarre reason still isn't handled correctly by Flycheck.
    (radian-protect-macros
      (cl-pushnew
       (cons
        (flycheck-rx-to-string
         `(and
           line-start
           (message "No JSON object could be decoded")
           line-end)
         'no-group)
        'error)
       (flycheck-checker-get 'json-python-json 'error-patterns)
       :test #'equal))))

;; Package `jsonnet-mode' provides a major mode for Jsonnet.

(use-package jsonnet-mode
  :mode
  (("\\.jsonnet\\'" . jsonnet-mode)
   ("\\.libsonnet\\'" . jsonnet-mode)))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.

(use-package pip-requirements)

;; Package `pkgbuild-mode' provides a major mode for PKGBUILD files
;; used by Arch Linux and derivatives.

(use-package pkgbuild-mode)

;; Package `ssh-config-mode' provides major modes for files in ~/.ssh.

(use-package ssh-config-mode)

;; Package `terraform-mode' provides major modes for Terraform
;; configuration files.

(use-package terraform-mode)

;; Package `toml-mode' provides a major mode for TOML.

(use-package toml-mode
  :mode "Pipfile\\'")

;; Package `yaml-mode' provides a major mode for YAML.

(use-package yaml-mode)


;;; Emails

;; Package `notmuch' is an interface to notmuch, a mail indexer on
;; top of xapian search engine.

(use-package notmuch
  :init
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-f-is-evil 't)
  (setq message-sendmail-envelope-from 'header)
  (setq sendmail-program "msmtp")

  :leader
  ("mm" #'notmuch))

;; Package `counsel-notmuch' integrates `counsel' with `notmuch'.

(use-package counsel-notmuch
  :after counsel
  :commands counsel-notmuch
  :leader
  ("m/" #'counsel-notmuch))


;;; Mode lines

;; Package `telephone-line' is a new implementation of powerline for emacs
;; with `evil' support, antialiased separators, and an easy configuration
;; language.

(use-package telephone-line)


;;; Themes

(defun gemacs--disable-theme ()
  "Disable theme propogation."
  (mapc #'disable-theme custom-enabled-themes))

(advice-add 'theme-dont-propagate :before #'gemacs--disable-theme)

(use-package kaolin-themes)


;;; Finalizing

;; Disable Emacs default behaviors.

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Always use y/n instead of yes/no.

(defalias 'yes-or-no-p 'y-or-n-p)

;; Also enable `evil-mode' for *Messages* which is created before
;; `evil-mode' is initialized.

(with-current-buffer (get-buffer "*Messages*")
  (turn-on-evil-mode))

;; Enable telephone-line as late as is humanly possible. This make sure
;; all features are available before `telephone-line' is initialized.

(use-feature telephone-line
  :demand t)

(use-feature telephone-line-config
  :after telephone-line
  :demand t

  :config
  (setq telephone-line-lhs
    '((nil    . (telephone-line-window-number-segment))
       (evil   . (telephone-line-evil-tag-segment))
       (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
       (nil    . (telephone-line-projectile-segment
                   telephone-line-buffer-segment))))

  (telephone-line-mode +1))

;; Enable color theme as late as is humanly possible. This reduces
;; frame flashing and other artifacts during startup.

(use-feature kaolin-themes
  :demand t
  :config
  (load-theme 'kaolin-valley-dark t))

(provide 'dot)
;;; dot.el ends here
