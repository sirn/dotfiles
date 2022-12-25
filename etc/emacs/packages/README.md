# Emacs Packages

## use-package

Use the following order for `use-package` and always leave a single line between keywords:

-   `straight`
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

```elisp
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

```elisp
(use-package markdown-mode
  :config
  (use-feature apheleia
    :demand t

    :config
    (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))))
    (add-hook 'markdown-mode-hook #'apheleia-mode)
```

## lsp-mode

`lsp-mode` is explicitly enabled per major-mode:

```elisp
(use-package typescript-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function lsp-format-buffer nil)
    (declare-function lsp-organize-imports nil)
    (declare-function gemacs--typescript-auto-format nil))

  :config
  (use-feature lsp-mode
    :demand t

    :config
    (defun gemacs--typescript-auto-format ()
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (add-hook 'before-save-hook #'lsp-organize-imports))

    (add-hook 'typescript-mode-hook #'lsp)
    (add-hook 'typescript-mode-hook #'gemacs--typescript-auto-format)))
```

## flycheck

`flycheck` is explicity enabled per major-mode:

```elisp
(use-feature sh-mode
  :config
  (use-feature flycheck
    :demand t

    :config
    (add-hook 'sh-mode-hook #'flycheck-mode)))
```
