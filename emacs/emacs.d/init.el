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
(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages
  '(better-defaults
    auto-complete
    fill-column-indicator
    magit
    helm))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Behavior
;; -----------------------------------------------------------------------------
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq inhibit-splash-screen t)

;; Modules
;; -----------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(setq helm-M-x-fuzzy-match t)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Window
;; -----------------------------------------------------------------------------
(when window-system
  (require 'whitespace)
  (global-whitespace-mode t)
  (custom-set-faces '(whitespace-space ((t (:foreground "#383838")))))
  (custom-set-faces '(whitespace-newline ((t (:foreground "#383838")))))
  (setq whitespace-line-column 80)

  (require 'fill-column-indicator)
  (define-globalized-minor-mode global-fci-mode
    fci-mode (lambda () (fci-mode 1)))
  (global-fci-mode 1)
  (setq fci-rule-column 80)
  (setq fci-rule-color "#FF0000")

  (require 'linum)
  (global-linum-mode 1)
  (custom-set-faces '(fringe ((t (:background "#2e2b2c")))))
  (custom-set-faces '(linum ((t (:foreground "#3f3f3f"
                                 :background "#2e2b2c")))))

  (load-theme 'carbonight t)
  (set-default-font "Inconsolata-dz-13"))
