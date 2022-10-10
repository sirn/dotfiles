;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package yaml-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function flymake-mode nil)
    (declare-function flymake-yamllint-setup nil)
    (declare-function gemacs--yaml-maybe-k8s nil)
    (declare-function gemacs--yaml-maybe-ansible nil))

  :config
  (use-feature apheleia
    :demand t

    :config
    (add-hook 'yaml-mode-hook #'apheleia-mode))

  (use-feature flymake
    :demand t

    :config
    (add-hook 'yaml-mode-hook #'flymake-mode))

  (use-feature flymake-yamllint
    :demand t

    :config
    (add-hook 'yaml-mode-hook #'flymake-yamllint-setup))

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
       (flymake-mode -1)))

  (add-hook 'yaml-mode-hook 'gemacs--yaml-maybe-ansible)
  (add-hook 'yaml-mode-hook 'gemacs--yaml-maybe-k8s))


(use-package ansible)


(use-package ansible-doc)


(use-package flymake-yamllint
  :straight (:host github :repo "shaohme/flymake-yamllint"))
