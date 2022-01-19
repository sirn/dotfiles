 ;; -*- lexical-binding: t -*-


(defun gemacs--treemacs-add-project-or-toggle ()
  "Initialize or toggle `treemacs-add-and-display-current-project'."
  (interactive)
  (when (projectile-project-p)
    (pcase (treemacs-current-visibility)
      ('visible (delete-window (treemacs-get-local-window)))
      ('exists  (treemacs-add-and-display-current-project))
      ('none    (treemacs-add-and-display-current-project)))))


(use-package treemacs
  ;; Not exposed via autoload by Treemacs
  :commands (treemacs-switch-workspace
              treemacs-create-workspace
              treemacs-rename-workspace
              treemacs-add-project-to-workspace)

  :leader
  ("tt" #'treemacs
   "tw" #'treemacs-switch-workspace
   "tC" #'treemacs-create-workspace
   "tE" #'treemacs-edit-workspaces
   "tR" #'treemacs-rename-workspace
   "tp" #'treemacs-add-project-to-workspace)

  :init
  (eval-when-compile
    (declare-function treemacs-current-visibility nil)
    (declare-function treemacs-get-local-window nil)
    (declare-function treemacs-select-window nil)
    (declare-function treemacs-add-and-display-current-project nil)
    (defvar treemacs-read-string-input))

  :config
  (setq treemacs-read-string-input 'from-minibuffer)
  (use-feature treemacs-evil
    :demand t)

  (use-feature treemacs-projectile
    :demand t)

  (use-feature treemacs-magit
    :demand t))


(use-package treemacs-evil)


(use-package treemacs-projectile)


(use-package treemacs-magit)
