# Emacs Packages

## use-package

Use the following order for `use-package`:

-   `straight`
-   `demand`
-   `defer`
-   `after`
-   `require`
-   `commands`
-   `bind`
-   `bind-keymap`
-   `leader`
-   `preface`
-   `init`
-   `config`

`:commands` must only be used when commands is not marked as autoloaded and must be accompany by a comment:

```elisp
(use-package treemacs
  ;; Not exposed via autoload by Treemacs
  :commands (treemacs-switch-workspace
              treemacs-create-workspace
              treemacs-rename-workspace
              treemacs-add-project-to-workspace))
```

## apheleia

`apheleia` configuration is deferred until major mode is loaded:

```elisp
(use-package markdown-mode
  :config
  (use-feature apheleia
    :config
    (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))))
```

## company

`company-backends` is configured when package is initialized, but defer until company is loaded:

```elisp
(use-package company-restclient
  :init
  (use-feature company
    :config
    (add-to-list 'company-backends 'company-restclient)))
```

## lsp-mode

`lsp-mode` is explicitly enabled per major-mode:

```elisp
(use-package typescript-mode
  :init
  (use-feature lsp-mode
    :init
    (add-hook 'typescript-mode-hook #'lsp)))
```
