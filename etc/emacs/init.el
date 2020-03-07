;; -*- lexical-binding: t -*-

;; This file wraps over an actual configuration for automatic
;; byte-compilation. This is roughly based on radian's initialization
;; file.

;;; ----------------------------------------------------------------------
;;; Early configurations

;; Prevent package.el from modifying this file.

(setq package-enable-at-startup nil)

;; Prevent Custom from modifying this file. This will be overriden
;; in the actual configuration file.

(setq custom-file (expand-file-name
		   (format "custom-%d-%d.el" (emacs-pid) (random))
		    temporary-file-directory))

;;; ----------------------------------------------------------------------
;;; File paths

(defvar dot-filename "dot.el")

;; Resolve the symlink to init.el so we can get the path of the actual
;; configuration file (`dot-filename').

(let ((target-file (file-symlink-p (or user-init-file
                                     load-file-name
                                     buffer-file-name))))

  (unless target-file
    (error "Init file %S is not a symlink" target-file))

  (defvar dot-el (expand-file-name
		  dot-filename
		  (file-name-directory target-file)))

  (unless (file-exists-p dot-el)
    (error "Configuration file %S does not exist" dot-filename)))

;;; ----------------------------------------------------------------------
;;; Loading

(let ((file-name-handler-alist nil)
      (load-prefer-newer t))

  (when (file-newer-than-file-p dot-el (concat dot-el "c"))
    (delete-file (concat dot-el "c")))

  (load (file-name-sans-extension dot-el) nil 'nomessage)
  (when (not (file-exists-p (concat dot-el "c")))
    (byte-compile-file dot-el)))
