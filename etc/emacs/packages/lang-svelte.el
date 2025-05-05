;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package svelte-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--svelte-auto-format nil))

  :init
  (defun gemacs--svelte-auto-format ()
    (add-hook 'before-save-hook #'gemacs--eglot-format-buffer -10 t))

  (add-hook 'svelte-mode-hook #'eglot-ensure)
  (add-hook 'svelte-mode-hook #'flycheck-mode)
  (add-hook 'svelte-mode-hook #'gemacs--svelte-auto-format)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))))
