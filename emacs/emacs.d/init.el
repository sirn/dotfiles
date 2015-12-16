;;; init.el --- Emacs configuration.
;;; Commentary:
;;; Code:


;; Paths
;; -----------------------------------------------------------------------------
(setq exec-path
      (append (list
               "/usr/local/bin"
               "/opt/local/bin"
               "/usr/bin"
               (expand-file-name "~/.rbenv/shims")
               (expand-file-name "~/.local/bin"))
              exec-path))

(add-to-list 'load-path "~/.dotfiles/emacs/emacs.d/site/")
(add-to-list 'custom-theme-load-path "~/.dotfiles/emacs/emacs.d/themes/")


;; Packages
;; -----------------------------------------------------------------------------

(defvar packages-list
  '(auto-complete
    better-defaults
    company
    fill-column-indicator
    flycheck
    helm
    keychain-environment
    magit
    sass-mode
    scss-mode
    yaml-mode))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (p packages-list)
  (unless (package-installed-p p)
    (package-install p)))


;; Behavior
;; -----------------------------------------------------------------------------

(setq auto-save-default nil)
(setq backup-inhibited t)
(setq inhibit-splash-screen t)


;; Modules
;; -----------------------------------------------------------------------------

;; Autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; Colors
(load-theme 'carbonight t)
(set-frame-font "Inconsolata-dz-11")

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  "Selectively turn off FCI for company-mode.  IGNORE all arguments."
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  "Selectively turn on FCI after company-mode.  IGNORE all arguments."
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; Fill-column-indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode
  fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 80)
(setq fci-rule-color "#FF0000")

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(defvar helm-M-x-fuzzy-match)
(setq helm-M-x-fuzzy-match t)

;; Keychain
(require 'keychain-environment)
(let ((original-shell (getenv "SHELL")))
  (and (setenv "SHELL" "/bin/bash")
       (keychain-refresh-environment)
       (setenv "SHELL" original-shell)))

;; Line numbers
(require 'linum)
(global-linum-mode 1)
(custom-set-faces '(fringe ((t (:background "#0c0c0c")))))
(custom-set-faces '(linum ((t (:foreground "#3f3f3f" :background "#0c0c0c")))))
(unless (window-system)
  (setq linum-format "%4d \u2502 "))

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Sass-mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

;; Scss-mode
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Whitespace-mode
(require 'whitespace)
(global-whitespace-mode t)
(custom-set-faces '(whitespace-space ((t (:foreground "#383838")))))
(custom-set-faces '(whitespace-newline ((t (:foreground "#383838")))))
(setq whitespace-style '(face trailing tabs))


;; Footer
;; -----------------------------------------------------------------------------

(provide 'init)
;;; init.el ends here
