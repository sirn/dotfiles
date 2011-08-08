;; Disable mouse integration.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Setup a proper exec path
(setq exec-path (append (list
                         "/usr/local/bin"
                         "/opt/local/bin"
                         "/usr/bin"
                         (expand-file-name "~/.rvm/gems/ruby-1.9.2-p180/bin")
                         (expand-file-name "~/.local/bin"))
                        exec-path))

;; Make sure load-path and package.el is properly setup.
(add-to-list 'load-path "~/.dotfiles/emacs/emacs.d/site/")
(when (not (require 'package nil t ))
  (load "package.el"))

;; Mermalade
(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Packages
(package-initialize)
(dolist (p '(color-theme auto-complete clojure-mode clojure-test-mode
                         starter-kit starter-kit-bindings starter-kit-js
                         starter-kit-ruby starter-kit-lisp sass-mode))
  (when (not (package-installed-p p))
    (package-install p)))

;; Behavior
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq indent-tabs-mode nil)
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
