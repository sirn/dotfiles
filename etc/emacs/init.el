;; -*- lexical-binding: t; no-native-compile: t -*-

;; This file wraps over an actual configuration for automatic loading
;; and byte-compilation.

;; --------------------------------------------------------------------------
;;; Early configurations

;; Prevent package.el from modifying this file.

(setq package-enable-at-startup nil)

;; Disable byte-compilation warnings from native-compiled packages
;; from being reported asynchronously into the UI.

(setq native-comp-async-report-warnings-errors nil)

;; Prevent Custom from modifying this file. This will be overriden
;; in the actual configuration file.

(setq custom-file (expand-file-name
                    (format "custom-%d-%d.el" (emacs-pid) (random))
                    temporary-file-directory))


;; --------------------------------------------------------------------------
;;; Pre-initialization

(require 'map)
(require 'subr-x)

(defgroup gemacs-hooks nil
  "Gemacs's hooks"
  :group 'gemacs
  :link '(url-link :tag "GitHub" "https://github.com/sirn/dotfiles"))

(defgroup gemacs nil
  "Gemacs's customization group."
  :prefix "gemacs-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/sirn/dotfiles"))


;; --------------------------------------------------------------------------
;;; GnuTLS

(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls)
    (defvar gnutls-trustfiles))

  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 3072)
  (when (< emacs-major-version 27)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

  (let ((cert "/usr/local/etc/libressl/cert.pem"))
    (when (file-exists-p cert)
      (add-to-list 'gnutls-trustfiles cert))))


;; --------------------------------------------------------------------------
;;; Hooks

(defcustom gemacs-after-init-hook nil
  "Hook run after at the very end of init."
  :group 'gemacs-hooks
  :type 'hook)


;; --------------------------------------------------------------------------
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
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

;; Key bindings are handled by general.el, which replaces both bind-key
;; and evil-leader; this is loaded early to allow use-package macro
;; to work correctly.

(use-package general)

(general-create-definer leader
    :keymaps 'override
    :states '(normal visual motion insert)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")


;; --------------------------------------------------------------------------
;;; Convenient helpers

;; The following macros are taken from Radian
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

;; A variant of use-package that doesn't attempt to load from straight.

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(defmacro gemacs-if-compiletime (cond then else)
  "Like `if', but COND is evaluated at compile time.
The macro expands directly to either THEN or ELSE, and the other
branch is not compiled. This can be helpful to deal with code
that uses functions only defined in a specific Emacs version."
  (declare (indent 2))
  (if (eval cond)
      then
    else))

(defmacro gemacs-when-compiletime (cond &rest body)
  "Like `when', but COND is evaluated at compile time.
BODY is only compiled if COND evaluates to non-nil. This can be
helpful to deal with code that uses functions only defined in a
specific Emacs version."
  (declare (indent 1))
  (when (eval cond)
    `(progn ,@body)))

(defmacro gemacs-flet (bindings &rest body)
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

(defmacro gemacs-protect-macros (&rest body)
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

(defun gemacs--path-join (path &rest segments)
  "Join PATH with SEGMENTS using `expand-file-name'.
First `expand-file-name' is called on the first member of
SEGMENTS, with PATH as DEFAULT-DIRECTORY. Then `expand-file-name'
is called on the second member, with the result of the first call
as DEFAULT-DIRECTORY, and so on. If no SEGMENTS are passed, the
return value is just PATH."
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

(defmacro gemacs--with-silent-message (regexps &rest body)
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
       (gemacs-flet ((defun message (format &rest args)
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

(defun gemacs--advice-silence-messages (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages.
This is an `:around' advice for many different functions."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))

(defun gemacs--advice-inhibit-message (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages. Unlike
`gemacs--advice-silence-messages', this function utilizes
`inhibit-message' for silencing message."
  (let ((inhibit-message t))
    (apply func args)))


;; --------------------------------------------------------------------------
;;; Saner defaults

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))


;; --------------------------------------------------------------------------
;;; Packages

(let ((loaded (mapcar
                #'file-name-sans-extension
                (delq nil (mapcar #'car load-history)))))
  (dolist (file (directory-files
                  "~/.dotfiles/etc/emacs/packages"
                  t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library loaded)
        (load library nil t)
        (push library loaded)))))

(with-current-buffer (get-buffer "*Messages*")
  (turn-on-evil-mode))


;; --------------------------------------------------------------------------
;;; Extra config

(let ((private-init-file "~/.dotpriv/etc/emacs/init.el"))
  (if (file-exists-p private-init-file)
    (load (file-name-sans-extension private-init-file))))


;; --------------------------------------------------------------------------
;;; Finalizing

(run-hooks 'gemacs-after-init-hook)


(provide 'init)
;;; init.el ends here
