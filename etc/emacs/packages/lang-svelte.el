;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package svelte-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--svelte-auto-format nil))

  :init
  (add-hook 'svelte-mode-hook #'eglot-ensure)
  (add-hook 'svelte-mode-hook #'flycheck-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))))
