 ;; -*- lexical-binding: t -*-


(defun gemacs--treemacs-add-project-or-toggle ()
  "Initialize or toggle `treemacs-add-and-display-current-project'."
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (delete-window (treemacs-get-local-window)))
    ('exists  (treemacs-select-window))
    ('none    (treemacs-add-and-display-current-project))))


(use-package treemacs
  :demand t

  :leader
  ("pt" #'gemacs--treemacs-add-project-or-toggle))


(use-package treemacs-evil
  :demand t)


(use-package treemacs-projectile
  :demand t)
