;; Initializes -------------------------------------------------------------

;; Setting up OS-specific variables
(setq is-w32 (string-equal system-type "windows-nt"))
(setq is-osx (string-equal system-type "darwin"))
(setq is-nix (string-equal system-type "gnu/linux"))

;; Setup path for elisp, binaries.
(setq exec-path
      (append (list
               "/usr/local/bin"
               "/opt/local/bin"
               "/usr/bin")
              exec-path))
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.dotfiles/elisp/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))
(unless is-w32
  ;; load some OS-stored elisp
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/"))

;; Essential ---------------------------------------------------------------

;; Navigation modes
;; - ido for navigation
;; - iswitchb is for buffer switching.
(require 'ido)
(require 'iswitchb)
(ido-mode t)
(iswitchb-mode 1)

;; Comint
(require 'comint)
(define-key comint-mode-map [(meta p)]
  'comint-previous-matching-input-from-input)
(define-key comint-mode-map [(meta n)]
  'comint-next-matching-input-from-input)
(define-key comint-mode-map [(control meta n)] 'comint-next-input)
(define-key comint-mode-map [(control meta p)] 'comint-previous-input)

;; YASnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.dotfiles/elisp/yasnippet/snippets")

;; Auto resize frames
(load "resize-frame.el")

;; Custom theme
(load "~/.dotfiles/elisp/custom-theme.el")

;; Minor modes -------------------------------------------------------------

;; Auto-completion
(require 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)

;; TextMate-like parenthesis matching, only enable this for Python-mode.
(autoload 'textmate-mode "emacs-textmate" "TextMate Pair Mode" t)
(add-hook 'python-mode-hook '(lambda () (textmate-mode 1)) t)

;; Major Modes -------------------------------------------------------------

;; Python-mode
(autoload 'python-mode "python-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; JavaScript-mode, only loads when open file ".js"
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; Erlang
(autoload 'erlang-mode "erlang.el" nil t)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;; Ruby-mode
(autoload 'ruby-mode "ruby-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

;; Markdown-mode, for editing article and such
(autoload 'markdown-mode "markdown-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))

;; RST-mode
(autoload 'rst-mode "rst.el" nil t)
(add-to-list 'auto-mode-alist '("\\.rst" . rst-mode))

;; Org-mode, organizing stuffs
(setq org-log-done t) ; so we know when it's finished
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Wikipedia-mode
(autoload 'wikipedia-mode "wikipedia-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.wiki" . wikipedia-mode))

;; Jinja2-mode
; turns out it's more annoying than useful without nxhtml.
;(autoload 'jinja-mode "jinja.el" nil t)
;(add-to-list 'auto-mode-alist '("\\.jinja2" . jinja-mode))

;; Repoze.bfg stuff
(add-to-list 'auto-mode-alist '("\\.jinja2" . html-mode))
(add-to-list 'auto-mode-alist '("\\.zcml" . xml-mode))

;; Personalizes ------------------------------------------------------------

;; Disable welcome screen
(setq inhibit-splash-screen t)

;; Only spaces, no mixing with tabs.
(setq-default indent-tabs-mode nil)

;; We don't want to clutter our working directory. No need to backup
;; almost everything I edited is in VCS anyway.
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Emacs window settings.
(when window-system
  ;; Themes
  (require 'color-theme)
  (color-theme-sunburst)
  ;; unclutter window
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  ;; restore Emacs behavior for Aquamacs
  (when (featurep 'aquamacs)
    (one-buffer-one-frame-mode 0))
  ;; Fonts settings
  (when is-w32
    (set-default-font
     "-outline-Consolas-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1"))
  (when is-osx
    (set-default-font "Menlo-12"))
  (when is-nix
    (set-default-font "DejaVu Sans Mono-9")
    (set-fontset-font nil 'thai '("Lomaputta"))))

;; Custom variables --------------------------------------------------------

;; Heh.
;; vim:ft=lisp 
