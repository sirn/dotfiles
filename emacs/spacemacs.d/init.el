;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.dotfiles/emacs/emacs.d/private/")
   dotspacemacs-configuration-layers
   '(
     ansible
     auto-completion
     better-defaults
     clojure
     emacs-lisp
     erlang
     erlang-rebar
     git
     go
     haskell
     html
     javascript
     keychain
     markdown
     org
     osx
     python
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-test-runner 'ruby-test)
     ruby-on-rails
     rust
     scala
     (shell :variables
            shell-default-shell 'eshell)
     syntax-checking
     terraform
     typescript
     vagrant
     version-control
     yaml)
   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-themes '(minimal)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)

   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols (display-graphic-p)
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  "Initialization function for user code. Called after Spacemacs init."

  ;; Disable the message about assigning $PATH in .profile instead of shell rc.
  (setq exec-path-from-shell-check-startup-files nil)

  ;; Store custom-set-variables in a separate file.
  (setq custom-file "~/.dotfiles/emacs/emacs.d/custom.el")
  (load custom-file))

(defun dotspacemacs/user-config ()
  "Configuration function for user code. Called after layers configuration."

  ;; Use jk keychord instead of ESC.
  (setq evil-escape-key-sequence "jk")

  ;; Disable sRGB in OS X to make Powerline colors appears "correct".
  ;; (Or maybe consistency wrong is a better word.)
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-default-separator 'utf-8)
  (spaceline-compile)

  ;; Fix vagrant-tramp broken on macOS by using custom TRAMP method and custom
  ;; vagrant-tramp-ssh binary.
  (add-to-list
   'tramp-methods
   `(,vagrant-tramp-method
     (tramp-login-env (("SHELL") ("/bin/sh")))
     (tramp-remote-shell "/bin/sh")
     (tramp-remote-shell-args ("-c"))
     (tramp-login-args (("%h")))
     (tramp-login-program ,(shell-quote-argument
                            (expand-file-name
                             "~/.dotfiles/bin/vagrant-tramp-ssh"))))))
