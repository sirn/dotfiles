;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package org
  :demand t
  :general
  (leader
    "o" #'gemacs--org-transient-menu)

  :init
  (require 'transient)

  (defun gemacs--org-find-file-in-directory ()
    "Find file in the org agenda directory."
    (interactive)
    (let ((default-directory org-directory))
      (call-interactively 'find-file)))

  (defun gemacs--org-init-hook ()
    "Initialize org by opening agenda and deleting other windows."
    (org-agenda-list)
    (delete-other-windows))

  (defun gemacs--org-agenda-mode-hook ()
    "Enable evil mode in org-agenda buffers and set to normal state."
    (with-eval-after-load 'evil
      (turn-on-evil-mode)
      (evil-normal-state)))

  (transient-define-prefix gemacs--org-transient-menu ()
    "Org commands."
    ["Capture"
     ("RET" "Capture to inbox" (lambda () (interactive) (org-capture nil "i")))]
    ["Views & Search"
     ("l" "Agenda list" org-agenda-list)
     ("t" "Todo list" org-todo-list)
     ("g" "QL view" org-ql-view)
     ("s" "Search in Org directory" org-ql-find-in-org-directory)
     ("f" "Search in current buffer" org-ql-find)]
    ["Files & Archive"
     ("o" "Open file in Org directory" gemacs--org-find-file-in-directory)
     ("k" "Archive subtree at point" org-archive-subtree)])

  :config
  (setq org-directory (expand-file-name "~/Org/"))
  (setq org-agenda-files (list org-directory))
  (setq org-archive-location (concat org-directory "archive/%s_archive::"))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords '((sequence "TODO(t)" "CURRENT(c)" "WAITING(w)" "|" "DONE(d!)")))
  (setq org-capture-templates
    `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
       "* TODO %?")))
  (setq org-startup-folded 'content)

  (unless (file-directory-p org-directory)
    (make-directory org-directory t))

  (add-hook 'gemacs-after-init-hook #'gemacs--org-init-hook)
  (add-hook 'org-agenda-mode-hook #'gemacs--org-agenda-mode-hook))


(use-package org-archive
  :demand t)


(use-package org-modern
  :demand t
  :after org
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))


(use-package org-ql
  :demand t
  :after org)


(use-package org-super-agenda
  :demand t
  :after org

  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-header-map nil)
  (setq org-super-agenda-groups
        '((:name "Due"
           :time-grid t
           :date today
           :todo "TODAY"
           :scheduled today
           :deadline today
           :deadline past
           :face error
           :order 1)
          (:name "Do First"
           :priority "A"
           :order 4)
          (:name "Do Next"
           :priority "B"
           :order 5)
          (:name "Do Later"
           :priority "C"
           :order 6)
          (:name "Do Someday"
           :priority<= "D"
           :order 7)
          (:name "Due Soon"
           :deadline future
           :order 8)
          (:name "Waiting"
           :todo "WAITING"
           :order 20)
          (:name "Inbox"
           :file-path "inbox.org"
           :order 30))))
