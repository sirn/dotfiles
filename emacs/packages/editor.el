(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(use-package avy
  :ensure t

  :commands
  (avy-goto-char
   avy-goto-char-2
   avy-goto-line)

  :init
  (eval-after-load 'evil-leader
    (evil-leader/set-key
      "jj" 'avy-goto-char
      "jJ" 'avy-goto-char-2
      "jl" 'avy-goto-line)))


(use-package color-identifiers-mode
  :defer 3
  :diminish color-identifiers-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function global-color-identifiers-mode nil))

  :config
  (global-color-identifiers-mode t))


(use-package company
  :defer 1
  :demand t
  :diminish company-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function global-company-mode nil))

  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match nil)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-align-annotations t)

  :bind
  (:map company-active-map
   ("TAB"      . company-complete-common-or-cycle)
   ([tab]      . company-complete-common-or-cycle)
   ("S-TAB"    . company-select-previous-or-abort)
   ([backtab]  . company-select-previous-or-abort)
   ([S-tab]    . company-select-previous-or-abort)
   ("C-p"      . company-select-previous-or-abort)
   ("C-n"      . company-select-next-or-abort)
   ("C-l"      . company-complete-selection))

  :config
  (global-company-mode t))


(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :ensure t

  :init
  (add-hook 'conf-mode-hook 'dtrt-indent-mode)
  (add-hook 'text-mode-hook 'dtrt-indent-mode)
  (add-hook 'prog-mode-hook 'dtrt-indent-mode))


(use-package eldoc
  :ensure t
  :diminish eldoc-mode)


(use-package flycheck
  :defer 1
  :diminish flycheck-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function global-flycheck-mode nil))

  :config
  (global-flycheck-mode t))


(use-package pandoc-mode
  :commands pandoc-mode
  :diminish pandoc-mode
  :ensure t

  :init
  (add-hook 'rst-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'pandoc-mode))


(use-package parinfer
  :diminish parinfer-mode
  :ensure t

  :commands
  (parinfer-mode
   parinfer-toggle-mode)

  :init
  (setq parinfer-extensions '(defaults pretty-parens evil smart-tab smart-yank))
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
  (add-hook 'common-lisp-mode-hook 'parinfer-mode)
  (add-hook 'scheme-mode-hook 'parinfer-mode)
  (add-hook 'lisp-mode-hook 'parinfer-mode))


(use-package prettier-js
  :commands prettier-js-mode
  :diminish prettier-js-mode
  :ensure t

  :init
  (setq prettier-js-args
        '("--trailing-comma" "all"
          "--tab-width" "4"))
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (add-hook 'markdown-mode-hook 'prettier-js-mode))


(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :ensure t

  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package rainbow-mode
  :commands rainbow-mode
  :diminish rainbow-mode
  :ensure t

  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))


(use-package smartparens
  :diminish smartparens-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function smartparens-global-mode nil))

  :init
  (eval-after-load 'evil-leader
    (evil-leader/set-key
      "s>" 'sp-forward-slurp-sexp
      "s<" 'sp-backward-slurp-sexp
      "sd" 'sp-splice-sexp))

  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))


(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function global-undo-tree-mode nil))

  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo")))
  (eval-after-load 'evil-leader
    (evil-leader/set-key
      "uv" 'undo-tree-visualize))

  :config
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))
  (global-undo-tree-mode t))


(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t

  :commands
  (yas-reload-all
   yas-minor-mode)

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


(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)
