 ;; -*- lexical-binding: t; no-native-compile: t -*-


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

  (use-feature treemacs-magit
    :demand t))


(use-package treemacs-evil)


(use-package treemacs-magit)
