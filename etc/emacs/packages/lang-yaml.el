(use-package ansible
  :commands ansible
  :straight t

  :preface
  (eval-when-compile
    (declare-function gr/ansible-enable-maybe nil))

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

  (add-hook 'yaml-mode-hook 'gr/ansible-enable-maybe)
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))


(use-package ansible-doc
  :diminish ansible-doc-mode
  :straight t

  :commands
  (ansible-doc
   ansible-doc-mode))


(use-package company-ansible
  :commands company-ansible
  :straight t

  :init
  (with-eval-after-load 'company
    (defun gr/setup-company-ansible ()
      (set (make-local-variable 'company-backends) '(company-ansible)))
    (add-hook 'ansible-hook 'gr/setup-company-ansible)))


(use-package yaml-mode
  :mode ("\\.\\(yaml|yml\\)\\'" . yaml-mode)
  :straight t

  :init
  (add-hook 'yaml-mode-hook 'flycheck-mode))
