;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package nix-mode
  :init
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))


(use-package nix-ts-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil))

  :hook
  ((nix-ts-mode . eglot-ensure)
   (nix-ts-mode . apheleia-mode)
   (nix-ts-mode . flymake-mode))

  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil"))))

  :config
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(nixpkgs-fmt . ("nixpkgs-fmt" inplace)))
    (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixpkgs-fmt))))
