(use-package ansible
  :commands ansible
  :straight t)


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


(use-package poly-yaml-mode
  :straight (poly-yaml-mode
              :type git
              :repo "https://git.sr.ht/~sirn/poly-yaml-mode")

  :init
  (with-eval-after-load 'fill-column-indicator
    (add-hook 'poly-yaml+sh-mode-hook 'fci-mode)
    (add-hook 'poly-yaml+ansible-mode-hook 'fci-mode))

  :mode
  ("\\.builds/.*\\.ya?ml\\'" . poly-yaml+sh-mode)
  ("\\.circleci/.*\\.ya?ml\\'" . poly-yaml+sh-mode)
  ("\\.build\\.ya?ml\\'" . poly-yaml+sh-mode)
  ("playbook\\.ya?ml\\'" . poly-yaml+ansible-mode)
  ("/ansible/.*\\.ya?ml\\'" . poly-yaml+ansible-mode)
  ("/\\(?:group\\|host\\)_vars/" . poly-yaml+ansible-mode))
