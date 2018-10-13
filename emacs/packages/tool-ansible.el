(use-package ansible
  :after yaml-mode
  :commands ansible
  :straight t

  :preface
  (eval-when-compile
    (defvar gr/ansible-filename-re)
    (declare-function gr/ansible-maybe-enable nil))

  :init
  (setq gr/ansible-filename-re ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")
  (defun gr/ansible-maybe-enable ()
    (and (stringp buffer-file-name)
         (string-match gr/ansible-filename-re buffer-file-name)))
  (defun gr/setup-ansible-maybe ()
    (when (gr/ansible-maybe-enable)
      (ansible t)))
  (add-hook 'yaml-mode-hook 'gr/setup-ansible-maybe)
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)

  :config
  (defun gr/ansible-reset-buffer-modified ()
    (set-buffer-modified-p nil))
  (advice-add 'ansible::decrypt-buffer :after 'gr/ansible-reset-buffer-modified))


(use-package ansible-doc
  :after ansible
  :diminish ansible-doc-mode
  :straight t

  :commands
  (ansible-doc
   ansible-doc-mode)

  :init
  (defun gr/setup-ansible-doc ()
    (ansible-doc-mode t))

  (add-hook 'ansible-hook 'gr/setup-ansible-doc))


(use-package company-ansible
  :after ansible
  :commands company-ansible
  :straight t

  :init
  (with-eval-after-load 'company
    (defun gr/setup-company-ansible ()
      (set (make-local-variable 'company-backends) '(company-ansible)))
    (add-hook 'ansible-hook 'gr/setup-company-ansible)))
