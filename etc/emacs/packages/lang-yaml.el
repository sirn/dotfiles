;; -*- lexical-binding: t -*-

(use-package yaml-mode)


(use-package ansible
  :preface
  (eval-when-compile
    (declare-function gemacs--ansible-maybe-enable nil))

  :init
  (defun gemacs--ansible-maybe-enable ()
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

  (add-hook 'yaml-mode-hook 'gemacs--ansible-maybe-enable))


(use-package ansible-doc)


(use-package company-ansible
  :init
  (use-feature company
    :config
    (defun gemacs--setup-company-ansible ()
      (set (make-local-variable 'company-backends) '(company-ansible)))
    (add-hook 'ansible-hook #'gemacs--setup-company-ansible)))
