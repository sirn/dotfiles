# Emacs Packages

## use-package

Use the following order for `use-package` and always leave a single line between keywords:

-   `demand`
-   `defer`
-   `after`
-   `require`
-   `general`
-   `commands`
-   `custom`
-   `preface`
-   `init`
-   `config`

`:commands` must only be used when commands is not marked as autoloaded and must be accompany by a comment:

``` elisp
(use-package treemacs
  ;; Not exposed via autoload by Treemacs
  :commands (treemacs-switch-workspace
              treemacs-create-workspace
              treemacs-rename-workspace
              treemacs-add-project-to-workspace))
```

`:defines` and `:functions` are not used as they are sometimes not evaluated early enough to silent compilation warnings.

## apheleia

`apheleia` is explicitly enabled per major-mode:

``` elisp
(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook #'apheleia-mode))
```

Any adjustments to `apheleia`'s variables should be done via `:config`:

``` elisp
(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook #'apheleia-mode)

  :config
  (use-package apheleia
    :config
    (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))))
```

## lsp-mode

`lsp-mode` is explicitly enabled per major-mode:

``` elisp
(use-package typescript-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--typescript-auto-format nil))

  :init
  (defun gemacs--typescript-auto-format ()
    (add-hook 'before-save-hook #'lsp-format-buffer)
    (add-hook 'before-save-hook #'lsp-organize-imports))

  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'typescript-mode-hook #'gemacs--typescript-auto-format))
```

`lsp-deferred` should be used instead of `lsp` to let it paths after it is initialized by `envrc`.

## tree-sitter

`tree-sitter` is explicitly enabled and hooks are to be moved into the relevant `-ts-mode`:

``` elisp
(use-package typescript-mode
  :init
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))

(use-package typescript-ts-mode
  :init
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred))
```

Note the hook is added to `-ts-mode-hook` in this case.

## flycheck

`flycheck` is explicitly enabled per major-mode:

``` elisp
(use-package sh-mode
  :init
  (add-hook 'sh-mode-hook #'flycheck-mode))
```
