;; Hack to install org with straight.
;; See also https://github.com/raxod502/straight.el

(use-package subr-x)


(use-package git
  :straight t

  :config
  (setq org-log-done t))


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
  :commands evil-org-mode
  :diminish evil-org-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function evil-org-set-key-theme nil)
    (declare-function evil-org-agenda-set-keys nil))

  :init
  (defun gr/setup-evil-org ()
    (require 'evil-org)
    (require 'evil-org-agenda)
    (evil-org-mode)
    (evil-org-set-key-theme '(navigation insert textobjects additional))
    (evil-org-agenda-set-keys))
  (add-hook 'org-mode-hook 'gr/setup-evil-org)
  (add-hook 'org-agenda-mode-hook 'gr/setup-evil-org))


(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)

  :preface
  (eval-when-compile
    (defvar org-agenda-files)
    (defvar org-archive-file-header-format)
    (defvar org-archive-location)
    (defvar org-capture-templates)
    (defvar org-default-notes-file)
    (defvar org-directory)
    (defvar org-highest-priority)
    (defvar org-lowest-priority)
    (defvar org-map-continue-from)
    (defvar org-refile-targets)
    (declare-function git-run nil)
    (declare-function org-element-at-point nil)
    (declare-function org-map-entries nil)
    (declare-function org-archive-subtree nil)
    (declare-function org-babel-do-load-languages nil))

  :init
  (when (file-directory-p "~/Nextcloud/Org")
    (add-hook 'after-init-hook
      '(lambda ()
         (org-agenda-list)))
    (setq initial-buffer-choice
      '(lambda ()
         (get-buffer org-agenda-buffer-name))))

  (setq org-directory "~/Nextcloud/Org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-archive-location "archive/%s_archive::datetree/* Archived")
  (setq org-lowest-priority ?F)
  (setq org-highest-priority ?A)
  (setq org-archive-file-header-format "")
  (setq org-agenda-files (list org-directory))
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "oa" 'org-agenda-list
      "oc" 'counsel-org-capture
      "of" 'counsel-org-goto-all))

  (setq org-capture-templates
    '(("i" "Inbox" entry (file+headline org-default-notes-file "Inbox")
        "* TODO %?\n  %t")
      ("c" "Capture" entry (file+headline org-default-notes-file "Inbox")
        "* TODO %?\n  %t\n  %i")))

  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from
         (org-element-property :begin (org-element-at-point))))
     "/DONE" 'agenda))

  :config
  (org-babel-do-load-languages
    'org-babel-load-languages '((python . t))))
