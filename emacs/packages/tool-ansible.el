(use-package ansible
  :after yaml-mode
  :commands ansible
  :ensure t

  :preface
  (eval-when-compile
    (defvar ansible-filename-re)
    (declare-function ansible-maybe-enable nil))

  :init
  (setq ansible-filename-re
        ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")

  (defun ansible-maybe-enable ()
    (and (stringp buffer-file-name)
        (string-match ansible-filename-re buffer-file-name)))

  (defun setup-ansible-maybe ()
    (when (ansible-maybe-enable)
      (ansible t)))

  (add-hook 'yaml-mode-hook 'setup-ansible-maybe)
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)

  :config
  (defun ansible-reset-buffer-modified ()
    (set-buffer-modified-p nil))
  (advice-add 'ansible::decrypt-buffer :after 'ansible-reset-buffer-modified))


(use-package ansible-doc
  :after ansible
  :diminish ansible-doc-mode
  :ensure t

  :commands
  (ansible-doc
   ansible-doc-mode)

  :init
  (defun setup-ansible-doc ()
    (ansible-doc-mode t))
  (add-hook 'ansible-hook 'setup-ansible-doc))


(use-package company-ansible
  :after ansible
  :commands company-ansible
  :ensure t

  :init
  (with-eval-after-load 'company
    (defun setup-company-ansible ()
      (set (make-local-variable 'company-backends) '(company-ansible)))
    (add-hook 'ansible-hook 'setup-company-ansible)))
