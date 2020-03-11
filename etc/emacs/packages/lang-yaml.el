;; -*- lexical-binding: t -*-

(use-package yaml-mode)


(use-package ansible
  :commands ansible

  :preface
  (eval-when-compile
    (declare-function gemacs--ansible-enable-maybe nil))

  :init
  (defun gr/ansible-enable-maybe ()
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
      (ansible t)))

  (add-hook 'yaml-mode-hook 'gemacs--ansible-enable-maybe))


(use-package ansible-doc
  :commands
  (ansible-doc
   ansible-doc-mode))


(use-package company-ansible
  :commands company-ansible

  :init
  (use-feature company
    :config
    (defun gemacs--setup-company-ansible ()
      (set (make-local-variable 'company-backends) '(company-ansible)))
    (add-hook 'ansible-hook #'gemacs--setup-company-ansible)))
