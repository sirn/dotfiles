;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package yaml-mode
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))


;; Builtin; tree-sitter
(use-package yaml-ts-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--yaml-maybe-k8s nil)
    (declare-function gemacs--yaml-maybe-ansible nil))

  :init
  (add-hook 'yaml-ts-mode-hook #'apheleia-mode)
  (add-hook 'yaml-ts-mode-hook #'flycheck-mode)

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
      (ansible +1)))

  (defun gemacs--yaml-maybe-k8s ()
    (when (and
            (stringp buffer-file-name)
            (string-match
              (rx ".yaml" string-end)
              buffer-file-name)
            (save-excursion
              (goto-char (point-min))
              (looking-at "\\(---\n\\)?apiVersion:")))
       (apheleia-mode -1)
       (flycheck-mode -1)))

  (add-hook 'yaml-ts-mode-hook 'gemacs--yaml-maybe-ansible)
  (add-hook 'yaml-ts-mode-hook 'gemacs--yaml-maybe-k8s))


(use-package ansible)


(use-package ansible-doc)
