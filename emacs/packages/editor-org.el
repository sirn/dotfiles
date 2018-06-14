(use-package evil-org
  :after evil
  :commands evil-org-mode
  :diminish evil-org-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function evil-org-set-key-theme nil)
    (declare-function evil-org-agenda-set-keys nil))

  :init
  (defun setup-evil-org ()
    (require 'evil-org)
    (require 'evil-org-agenda)
    (evil-org-mode)
    (evil-org-set-key-theme '(navigation insert textobjects additional))
    (evil-org-agenda-set-keys))
  (add-hook 'org-mode-hook 'setup-evil-org))


(use-package helm-org-rifle
  :after helm
  :commands helm-org-rifle
  :ensure t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "h/" 'helm-org-rifle-org-directory
      "hc/" 'helm-org-rifle)))


(use-package org
  :mode ("\\.org\\'" . org-mode)

  :init
  (setq org-directory (expand-file-name "~/Dropbox/Documents/Org/")))
