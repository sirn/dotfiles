;; Hack to install org with straight.
;; See also https://github.com/raxod502/straight.el

(use-package subr-x)


(use-package git
  :straight t)


(defun org-git-version ()
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/"
                   user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/"
                   user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))


(provide 'org-version)


(use-package evil-org
  :after (evil org)
  :commands evil-org-mode
  :diminish evil-org-mode
  :straight t

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


(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)

  :preface
  (eval-when-compile
    (declare-function git-run nil))

  :init
  (setq org-directory (expand-file-name "~/Dropbox/Documents/Org/")))
