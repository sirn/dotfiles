;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package flycheck-eglot
  :config
  (global-flycheck-eglot-mode t))


(use-package eglot
  :general
  (leader
    "ll" #'gemacs--eglot-transient-menu)

  :custom
  (eglot-autoshutdown t)

  :preface
  (eval-when-compile
    (declare-function eglot-current-server nil)
    (declare-function eglot-format-buffer nil)
    (declare-function eglot-shutdown nil)
    (declare-function gemacs--advice-eglot-shutdown-project nil)
    (declare-function gemacs--eglot-format-buffer nil)
    (declare-function gemacs--eglot-organize-imports nil))

  :init
  (require 'transient)

  (transient-define-prefix gemacs--eglot-transient-menu ()
    "Eglot commands."
    ["Code Actions"
     ("l" "Code actions" eglot-code-actions)
     ("r" "Rename symbol" eglot-rename)
     ("f" "Format buffer" eglot-format-buffer)
     ("o" "Organize imports" eglot-code-action-organize-imports)]
    ["Find"
     ("i" "Find implementation" eglot-find-implementation)
     ("d" "Find declaration" eglot-find-declaration)
     ("t" "Find type definition" eglot-find-typeDefinition)]
    ["Server"
     ("'" "Reconnect" eglot-reconnect)])

  (defun gemacs--eglot-format-buffer ()
    (eglot-format-buffer))

  (defun gemacs--eglot-organize-imports ()
    (call-interactively 'eglot-code-action-organize-imports))

  :config
  (use-package project
    :config
    (defun gemacs--advice-eglot-shutdown-project (orig-fun &rest args)
      (let* ((pr (project-current t))
             (default-directory (project-root pr)))
        (when-let ((server (eglot-current-server)))
          (ignore-errors (eglot-shutdown server)))
        (apply orig-fun args)))

    (advice-add 'project-kill-buffers :around #'gemacs--advice-eglot-shutdown-project))

  (use-package flycheck-eglot
    :demand t))


(use-package xref
  :general
  (leader
    "lx" #'gemacs--xref-transient-menu)

  :init
  (require 'transient)

  (transient-define-prefix gemacs--xref-transient-menu ()
    "Xref commands."
    ["Navigation"
     ("d" "Find definitions" xref-find-definitions)
     ("r" "Find references" xref-find-references)
     ("a" "Find apropos" xref-find-apropos)
     ("b" "Go back" xref-go-back)
     ("f" "Go forward" xref-go-forward)]))
