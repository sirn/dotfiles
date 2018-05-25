(setq ansible-filename-re
      ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")

(defun custom/ansible-maybe-enable ()
  (and (stringp buffer-file-name)
       (string-match ansible-filename-re buffer-file-name)))

(defun custom/setup-ansible-maybe ()
  (when (custom/ansible-maybe-enable)
    (ansible t)))

(defun custom/setup-ansible-doc ()
  (ansible-doc-mode t))

(defun custom/setup-company-ansible ()
  (set (make-local-variable 'company-backends) '(company-ansible)))

(defun custom/ansible-reset-buffer-modified ()
  (set-buffer-modified-p nil))

(req-package ansible
  :require yaml-mode
  :commands ansible
  :init
  (progn
    (add-hook 'yaml-mode-hook 'custom/setup-ansible-maybe)
    (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))
  :config
  (advice-add 'ansible::decrypt-buffer
              :after 'custom/ansible-reset-buffer-modified))

(req-package ansible-doc
  :require ansible
  :diminish ansible-doc-mode
  :commands (ansible-doc ansible-doc-mode)
  :init
  (progn
    (add-hook 'ansible-hook 'custom/setup-ansible-doc)))

(req-package company-ansible
  :require (ansible company)
  :commands company-ansible
  :init
  (progn
    (add-hook 'ansible-hook 'custom/setup-company-ansible)))
