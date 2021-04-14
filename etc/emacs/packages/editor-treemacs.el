 ;; -*- lexical-binding: t -*-


(defun gemacs--treemacs-add-project-or-toggle ()
  "Initialize or toggle `treemacs-add-and-display-current-project'."
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (delete-window (treemacs-get-local-window)))
    ('exists  (treemacs-add-and-display-current-project))
    ('none    (treemacs-add-and-display-current-project))))


(use-package treemacs
  :demand t

  :init
  (eval-when-compile
    (declare-function treemacs-current-visibility nil)
    (declare-function treemacs-get-local-window nil)
    (declare-function treemacs-select-window nil)
    (declare-function treemacs-add-and-display-current-project nil))

  :leader
  ("pt" #'gemacs--treemacs-add-project-or-toggle))


(use-package treemacs-evil
  :demand t)


(use-package treemacs-projectile
  :demand t)
