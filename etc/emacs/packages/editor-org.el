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
  (setq org-todo-keywords '((sequence "TODO(t)" "CURRENT(c)" "ONHOLD(h)" "WAITING(w)" "|" "DONE(d!)" "DROPPED(x!)")))
  (setq org-capture-templates
    `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox")
       "* TODO %?")))
  (setq org-startup-folded 'content)

  (unless (file-directory-p org-directory)
    (make-directory org-directory t))

  :hook
  (org-agenda-mode . gemacs--org-agenda-mode-hook))


(use-package org-archive)


(use-package org-modern
  :after org

  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))

  :config
  (with-eval-after-load 'modus-themes
    (modus-themes-with-colors
      (setq org-modern-todo-faces
        `(("CURRENT" . (:inherit org-modern-todo :foreground ,yellow-warmer))
          ("WAITING" . (:inherit org-modern-todo :foreground ,cyan-faint))
          ("ONHOLD" . (:inherit org-modern-todo :foreground ,fg-dim)))))))


(use-package org-ql
  :after org)


(use-package org-super-agenda
  :after org

  :custom
  (org-super-agenda-header-map nil)

  :hook (after-init . org-super-agenda-mode)

  :config
  (with-eval-after-load 'modus-themes
    (modus-themes-with-colors
      (setq org-super-agenda-groups
        `((:name "Current"
           :todo "CURRENT"
           :order 1
           :face (:foreground ,yellow-faint))
          (:name "Prioritized"
           :and (:priority "A" :date today)
           :order 2)
          (:name "Waiting"
           :todo "WAITING"
           :face (:foreground ,cyan-faint)
           :order 10)
          (:name "Past Due"
           :scheduled past
           :deadline past
           :order 4)
          (:name "Due"
           :time-grid t
           :order 9)
          (:name "Today"
           :date t
           :order 5))))))
