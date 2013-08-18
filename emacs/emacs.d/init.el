;; Setup a proper exec path
(setq exec-path (append (list
                         "/usr/local/bin"
                         "/opt/local/bin"
                         "/usr/bin"
                         (expand-file-name "~/.rbenv/shims")
                         (expand-file-name "~/.local/bin"))
                        exec-path))

;; Make sure load-path is properly setup.
(add-to-list 'load-path "~/.dotfiles/emacs/emacs.d/site/")

;; Packages
(package-initialize)
(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/") t)
(when (not package-archive-contents) (package-refresh-contents))
(defvar packages '(better-defaults color-theme auto-complete))

;; Make sure all packages are installed
(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Behavior
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq inhibit-splash-screen t)

;; Modules
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; Window
(when window-system
  (require 'whitespace)
  (global-whitespace-mode t)
  (set-default-font "Inconsolata-dz-12"))

;; Custom
(custom-set-variables
 '(safe-local-variable-values
   (quote ((whitespace-line-column . 80)
           (lexical-binding . t)))))
