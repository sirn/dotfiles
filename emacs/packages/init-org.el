(defun custom/setup-evil-org ()
  (require 'evil-org)
  (require 'evil-org-agenda)
  (evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional))
  (evil-org-agenda-set-keys))

(req-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (progn
    (setq org-directory (expand-file-name "~/Dropbox/Documents/Org/"))))

(req-package evil-org
  :require evil
  :commands evil-org-mode
  :diminish evil-org-mode
  :init
  (add-hook 'org-mode-hook 'custom/setup-evil-org))

(req-package helm-org-rifle
  :commands (helm-org-rifle)
  :init
  (evil-leader/set-key
    "h/" 'helm-org-rifle-org-directory
    "hc/" 'helm-org-rifle))
