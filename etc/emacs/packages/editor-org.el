;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package org
  :general
  (leader
    "go" #'gemacs--org-transient-menu)

  :init
  (require 'transient)

  (defun gemacs--org-find-file-in-directory ()
    "Find file in the org agenda directory."
    (interactive)
    (let ((default-directory gemacs--org-agenda-dir))
      (call-interactively 'find-file)))

  (transient-define-prefix gemacs--org-transient-menu ()
    "Org commands."
    ["Capture & Agenda"
     ("RET" "Capture to inbox" (lambda () (interactive) (org-capture nil "i")))
     ("l" "Agenda list" org-agenda-list)
     ("t" "Todo list" org-todo-list)]
    ["Open Files"
     ("o" "Open file in Org directory" gemacs--org-find-file-in-directory)])

  :config
  (setq gemacs--org-agenda-dir (expand-file-name "~/Org/"))
  (setq org-agenda-files (list gemacs--org-agenda-dir))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))
  (setq org-capture-templates
    `(("i" "Inbox" entry (file+headline ,(concat gemacs--org-agenda-dir "inbox.org") "Inbox")
       "* TODO %?"))))
