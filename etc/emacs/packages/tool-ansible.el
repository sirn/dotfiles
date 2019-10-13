(use-package ansible
  :commands ansible
  :straight t)


(use-package ansible-doc
  :after ansible
  :diminish ansible-doc-mode
  :straight t

  :commands
  (ansible-doc
   ansible-doc-mode))


(use-package company-ansible
  :after ansible
  :commands company-ansible
  :straight t

  :init
  (with-eval-after-load 'company
    (defun gr/setup-company-ansible ()
      (set (make-local-variable 'company-backends) '(company-ansible)))
    (add-hook 'ansible-hook 'gr/setup-company-ansible)))


(use-package poly-ansible
  :after polymode
  :straight t

  :preface
  (eval-when-compile
    (defvar pm-inner/jinja2 nil))

  :mode
  ("playbook\\.ya?ml\\'" . poly-ansible-mode)
  ("/ansible/.*\\.ya?ml\\'" . poly-ansible-mode)
  ("/\\(?:group\\|host\\)_vars/" . poly-ansible-mode)

  :init
  (with-eval-after-load 'fill-column-indicator
    (add-hook 'ansible-hook 'fci-mode))

  :config
  (setq pm-inner/jinja2
    (pm-inner-chunkmode :mode #'jinja2-mode
                        :head-matcher "{[%{#][+-]?"
                        :tail-matcher "[+-]?[%}#]}"
                        :head-mode 'body
                        :tail-mode 'body
                        :head-adjust-face nil
                        :tail-adjust-face nil)))
