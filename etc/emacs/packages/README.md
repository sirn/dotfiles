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

`apheleia` is explicitly enabled per major-mode:

```elisp
(use-package markdown-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (defvar apheleia-mode-alist))

  :config
  (use-feature apheleia
    :demand t
    :config
    (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))))
    (add-hook 'markdown-mode-hook #'apheleia-mode)
```

## eglot

`eglot` is explicitly enabled per major-mode:

```elisp
(use-package typescript-mode
  :preface
  (eval-when-compile
    (declare-function eglot-ensure nil)
    (declare-function gemacs--typescript-auto-format nil)
    (defvar eglot-server-programs))

  :config
  (use-feature eglot
    :demand t
    :config
    (defun gemacs--typescript-auto-format ()
      (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'gemacs--eglot-organize-imports nil t))

    (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server")))
    (add-hook 'typescript-mode-hook #'eglot-ensure)
    (add-hook 'typescript-mode-hook #'gemacs--typescript-auto-format)))
```

## flymake

`flymake` is explicity enabled per major-mode:

```elisp
(use-feature sh-mode
  :config
  (use-feature flymake-mode
    :demand t
    :preface
    (eval-when-compile
      (declare-function flymake-mode nil)))

    :config
    (add-hook 'sh-mode-hook #'flymake-mode)))
```
