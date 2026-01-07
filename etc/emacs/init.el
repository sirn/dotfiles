;; -*- lexical-binding: t; no-native-compile: t -*-

;; This file wraps over an actual configuration for automatic loading
;; and byte-compilation.

(setq gc-cons-threshold most-positive-fixnum)

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

(require 's)
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
;;; System

(let ((default-shell (expand-file-name "~/.emacs.d/bin/shell")))
  (setenv "SHELL" default-shell)
  (setq sh-shell-file default-shell)
  (setq shell-file-name default-shell))

;; Inherit environment variables from shell on macOS
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :demand t
  :config
  (exec-path-from-shell-initialize))


;; --------------------------------------------------------------------------
;;; Package management

(eval-when-compile
  (defvar use-package-always-defer)
  (defvar use-package-always-ensure)
  (defvar use-package-compute-statistics))

(setq use-package-always-defer t)
(setq use-package-always-ensure nil)
(setq use-package-compute-statistics nil)

(eval-when-compile
  (require 'use-package)
  (require 'el-patch))

(setq el-patch-warn-on-eval-template nil)


;; --------------------------------------------------------------------------
;;; Convenient helpers

;; The following macros are taken from Radian
;; https://github.com/radian-software/radian/blob/242c55c/emacs/radian.el

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
;;; Early packages

;; SQLite3 dynamic module bindings
(use-package sqlite3
  :demand t)

;; Keybinding framework (replaces bind-key and evil-leader)
(eval-when-compile
  (use-package general
    :demand t)

  (general-create-definer leader
    :keymaps 'override
    :states '(normal visual motion insert)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))


;; --------------------------------------------------------------------------
;;; Saner defaults

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep ~/.emacs.d clean by relocating files to etc/ and var/
(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))


;; --------------------------------------------------------------------------
;;; Packages

(let ((emacs-bin-deps-dir "~/.emacs.d/var/emacs-bin-deps"))
  (if (file-directory-p emacs-bin-deps-dir)
    (add-to-list 'exec-path (expand-file-name emacs-bin-deps-dir) t)))

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
;;; Finalizing

(run-hooks 'gemacs-after-init-hook)

(add-hook 'emacs-startup-hook
  (lambda () (setq gc-cons-threshold (* 16 1024 1024))))


(provide 'init)
;;; init.el ends here
