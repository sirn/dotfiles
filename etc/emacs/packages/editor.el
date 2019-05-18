(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(use-package avy
  :straight t

  :commands
  (avy-goto-char
   avy-goto-char-2
   avy-goto-line)

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "jj" 'avy-goto-char
      "jJ" 'avy-goto-char-2
      "jl" 'avy-goto-line)))


(use-package company
  :demand t
  :diminish company-mode
  :straight t

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


(use-package company-prescient
  :after (company prescient)
  :straight t

  :preface
  (eval-when-compile
    (declare-function company-prescient-mode nil))

  :config
  (company-prescient-mode t))


(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :straight t

  :init
  (add-hook 'conf-mode-hook 'dtrt-indent-mode)
  (add-hook 'text-mode-hook 'dtrt-indent-mode)
  (add-hook 'prog-mode-hook 'dtrt-indent-mode))


(use-package eldoc
  :straight t
  :diminish eldoc-mode)


(use-package flycheck
  :diminish flycheck-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-flycheck-mode nil))

  :init
  (add-hook 'markdown-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flycheck-mode)

  :config
  (global-flycheck-mode t))


(use-package origami
  :diminish origami-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-origami-mode nil))

  :config
  (global-origami-mode t)
  (evil-define-key 'normal prog-mode-map
    (kbd "TAB")   'origami-toggle-node
    (kbd "M-TAB") 'origami-toggle-all-nodes))


(use-package pandoc-mode
  :commands pandoc-mode
  :diminish pandoc-mode
  :straight t

  :init
  (add-hook 'rst-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'pandoc-mode))


(use-package parinfer
  :diminish parinfer-mode
  :straight t

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
  :straight (prettier-js :type git
                         :host github
                         :repo "prettier/prettier-emacs"
                         :fork (:host github
                                      :repo "sirn/prettier-emacs"
                                      :branch "rcs-diff-program"))

  :preface
  (eval-when-compile
    (defvar prettier-js-args)
    (defvar prettier-js-diff-command))

  :init
  (setq prettier-js-args
        '("--trailing-comma" "all"
          "--tab-width" "4"))
  (when (executable-find "gdiff")
    (setq prettier-js-diff-command "gdiff"))
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (add-hook 'markdown-mode-hook 'prettier-js-mode))


(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :straight t

  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package rainbow-mode
  :commands rainbow-mode
  :diminish rainbow-mode
  :straight t

  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))


(use-package smartparens
  :diminish smartparens-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function smartparens-global-mode nil))

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "s>" 'sp-forward-slurp-sexp
      "s<" 'sp-backward-slurp-sexp
      "sd" 'sp-splice-sexp))

  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (add-hook 'clojure-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'common-lisp-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'scheme-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'lisp-mode-hook 'turn-off-smartparens-mode))


(use-package undo-tree
  :diminish undo-tree-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function global-undo-tree-mode nil))

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "uv" 'undo-tree-visualize))

  :config
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))
  (global-undo-tree-mode t))


(use-package visual-regexp
  :commands (vr/replace vr/query-replace)
  :straight t

  :init
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace))


(use-package visual-regexp-steroids
  :after visual-regexp
  :straight t)


(use-package yasnippet
  :diminish yas-minor-mode
  :straight t

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
  :straight t)
