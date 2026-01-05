;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package python
  :custom
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil)

  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil))

  :hook
  ((python-ts-mode . eglot-ensure)
   (python-ts-mode . flymake-mode)
   (python-ts-mode . apheleia-mode))

  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  (cond
    ((executable-find "python3") (setq python-shell-interpreter "python3"))
    ((executable-find "python2") (setq python-shell-interpreter "python2"))
    (t (setq python-shell-interpreter "python")))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
      `(python-ts-mode . ,(eglot-alternatives
                            '(("pylsp")
                              ("pyright-langserver" "--stdio")))))

    (setq-default eglot-workspace-configuration
      '((pylsp
          (plugins
            (black (enabled . t))
            (flake8 (enabled . t))
            (pycodestyle (enabled . nil))
            (pyflakes (enabled . t)))))))

  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-mode-alist '(python-ts-mode . (isort black)))))
