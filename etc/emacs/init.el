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
  (defvar use-package-always-defer)
  (defvar use-package-compute-statistics))

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq use-package-compute-statistics nil)

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
