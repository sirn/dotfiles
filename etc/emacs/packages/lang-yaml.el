;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package yaml-mode
  :init
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))


;; Builtin; tree-sitter
(use-package yaml-ts-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--yaml-maybe-apheleia nil)
    (declare-function gemacs--yaml-maybe-ansible nil)
    (declare-function apheleia-mode nil)
    (declare-function eglot-ensure nil)
    (declare-function flycheck-mode nil)
    (declare-function ansible nil))

  :init
  (add-hook 'yaml-ts-mode-hook #'eglot-ensure)
  (add-hook 'yaml-ts-mode-hook #'flycheck-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(yaml-ts-mode . ("yaml-language-server" "--stdio"))))

  (defun gemacs--yaml-maybe-ansible ()
    (when (and
            (stringp buffer-file-name)
            (string-match
              (rx (or "playbook"
                    (seq (or (seq (or "group" "host") "_vars")
                           "ansible"
                           "roles")
                      "/" (* nonl)))
                ".y" (? "a") "ml")
              buffer-file-name))
      (ansible-mode +1)))

  ;; only activate apheleia when not working with k8s manifests
  ;; due to k8s' annoying indention that nobody else uses
  ;; (and also the mess that is helm)
  (defun gemacs--yaml-maybe-apheleia ()
    (when (stringp buffer-file-name)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^apiVersion:" nil t)
          (apheleia-mode -1)
          (apheleia-mode +1)))))

  (add-hook 'yaml-ts-mode-hook 'gemacs--yaml-maybe-ansible)
  (add-hook 'yaml-ts-mode-hook 'gemacs--yaml-maybe-apheleia))


(use-package ansible)


(use-package ansible-doc)
