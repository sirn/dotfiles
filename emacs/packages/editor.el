(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(req-package avy
  :require evil-leader
  :commands (avy-goto-char avy-goto-char-2 avy-goto-line)
  :init
  (evil-leader/set-key
    "jj" 'avy-goto-char
    "jJ" 'avy-goto-char-2
    "jl" 'avy-goto-line))

(req-package color-identifiers-mode
  :diminish color-identifiers-mode
  :init
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

(req-package company
  :diminish company-mode
  :init
  (progn
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 2)
    (setq company-require-match nil)
    (setq company-dabbrev-downcase nil)
    (setq company-selection-wrap-around t)
    (setq company-tooltip-flip-when-above t)
    (setq company-tooltip-align-annotations t))
  :config
  (progn
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map [tab] 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous-or-abort)
    (define-key company-active-map [S-tab] 'company-select-previous-or-abort)
    (global-company-mode t)))

(req-package diminish)

(req-package dtrt-indent
  :diminish dtrt-indent-mode
  :init
  (add-hook 'conf-mode-hook 'dtrt-indent-mode)
  (add-hook 'text-mode-hook 'dtrt-indent-mode)
  (add-hook 'prog-mode-hook 'dtrt-indent-mode))

(req-package eldoc
  :diminish eldoc-mode)

(req-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(req-package linum
  :require evil-leader
  :commands global-linum-mode
  :init
  (progn
    (setq linum-format "%4d ")
    (evil-leader/set-key
      "tl" 'global-linum-mode)))

(req-package linum-relative
  :require (evil-leader linum)
  :config
  (progn
    (setq linum-relative-current-symbol "")
    (setq linum-relative-format "%4s ")
    (linum-relative-mode)
    (evil-leader/set-key
      "tL" 'linum-relative-toggle)))

(req-package pandoc-mode
  :diminish pandoc-mode
  :commands pandoc-mode
  :init
  (progn
    (add-hook 'rst-mode-hook 'pandoc-mode)
    (add-hook 'markdown-mode-hook 'pandoc-mode)))

(req-package parinfer
  :commands (parinfer-mode parinfer-toggle-mode)
  :init
  (progn
    (setq parinfer-extensions '(defaults pretty-parens evil smart-tab smart-yank))
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(req-package prettier-js
  :diminish prettier-js-mode
  :commands prettier-js-mode
  :init
  (progn
    (add-hook 'css-mode-hook 'prettier-js-mode)
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'json-mode-hook 'prettier-js-mode)
    (add-hook 'markdown-mode-hook 'prettier-js-mode))
  :config
  (setq prettier-js-args
        '("--trailing-comma" "all"
          "--tab-width" "4")))

(req-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package rainbow-mode
  :commands rainbow-mode
  :diminish rainbow-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

(req-package smartparens
  :require evil-leader
  :diminish smartparens-mode
  :init
  (evil-leader/set-key
    "s>" 'sp-forward-slurp-sexp
    "s<" 'sp-backward-slurp-sexp
    "sd" 'sp-splice-sexp)
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))

(req-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo")))
    (unless (file-exists-p "~/.emacs.d/undo")
      (make-directory "~/.emacs.d/undo")))
  :config
  (progn
    (global-undo-tree-mode)))

(req-package yasnippet
  :commands (yas-reload-all yas-minor-mode)
  :diminish yas-minor-mode
  :init
  (add-hook 'elixir-mode-hook 'yas-minor-mode)
  (add-hook 'erlang-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'js2-mode-hook 'yas-minor-mode)
  (add-hook 'latex-mode-hook 'yas-minor-mode)
  (add-hook 'makefile-mode-hook 'yas-minor-mode)
  (add-hook 'php-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'rust-mode-hook 'yas-minor-mode)
  (add-hook 'sql-mode-hook 'yas-minor-mode)
  (add-hook 'terraform-mode-hook 'yas-minor-mode)
  (add-hook 'typescript-mode-hook 'yas-minor-mode)
  (add-hook 'yaml-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(req-package yasnippet-snippets
  :require yasnippet)
