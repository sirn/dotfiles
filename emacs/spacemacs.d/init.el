;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
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
     typescript
     vagrant
     version-control
     yaml
     )
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
                               :size 13
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
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code. Called after Spacemacs init."

  ;; Disable the message about assigning $PATH in .profile instead of shell rc.
  (setq exec-path-from-shell-check-startup-files nil)

  ;; Enable Racer for Rust auto-completion.
  (setq-default rust-enable-racer t))

(defun dotspacemacs/user-config ()
  "Configuration function for user code. Called after layers configuration."

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

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (minitest hide-comnt powerline rake pcre2el org alert log4e gntp markdown-mode json-mode json-snatcher json-reformat js2-mode parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter pos-tip flycheck flx evil-unimpaired magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree sbt-mode scala-mode diminish web-completion-data dash-functional tern go-mode ghc haskell-mode company hydra inflections edn multiple-cursors paredit peg eval-sexp-fu highlight cider seq spinner queue pkg-info clojure-mode epl rust-mode inf-ruby bind-map bind-key yasnippet packed anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup package-build pug-mode yapfify uuidgen tide typescript-mode py-isort osx-dictionary org-projectile org-download mwim livid-mode skewer-mode simple-httpd live-py-mode link-hint intero hlint-refactor helm-hoogle git-link eyebrowse evil-visual-mark-mode evil-ediff eshell-z dumb-jump company-ghci column-enforce-mode clojure-snippets cargo yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe vagrant-tramp vagrant use-package tss toml-mode toc-org tagedit spacemacs-theme spaceline smooth-scrolling smeargle slim-mode shm shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder restart-emacs rbenv rainbow-delimiters racer quelpa pyvenv pytest pyenv-mode py-yapf projectile-rails popwin pip-requirements persp-mode pbcopy paradox page-break-lines osx-trash orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file noflet neotree multi-term move-text mmm-mode minimal-theme markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme less-css-mode launchctl keychain-environment js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator feature-mode fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eshell-prompt-extras esh-help erlang ensime emmet-mode elisp-slime-nav diff-hl define-word cython-mode company-web company-tern company-statistics company-racer company-quickhelp company-go company-ghc company-cabal company-anaconda coffee-mode cmm-mode clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby bundler buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((web-mode-code-indent-offset . 2)
     (web-mode-css-indent-offset . 2)
     (web-mode-markup-indent-offset . 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
