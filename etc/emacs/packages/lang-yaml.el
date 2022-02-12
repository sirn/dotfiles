;; -*- lexical-binding: t -*-

(use-package yaml-mode
  :preface
  (eval-when-compile
    (declare-function gemacs--yaml-maybe-k8s nil)
    (declare-function gemacs--yaml-maybe-ansible nil))

  :init
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

  (add-hook 'yaml-mode-hook 'gemacs--yaml-maybe-ansible)
  (add-hook 'yaml-mode-hook 'gemacs--yaml-maybe-k8s))


(use-package ansible)


(use-package ansible-doc)


(use-package company-ansible
  :preface
  (eval-when-compile
    (declare-function gemacs--company-ansible-setup nil))

  :init
  (use-feature company
    :config
    (defun gemacs--company-ansible-setup ()
      (set (make-local-variable 'company-backends) '(company-ansible)))

    (add-hook 'ansible-hook #'gemacs--company-ansible-setup)))
