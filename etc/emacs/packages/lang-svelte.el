;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package svelte-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--svelte-auto-format nil))

  :hook
  ((svelte-mode . eglot-ensure)
   (svelte-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))))
