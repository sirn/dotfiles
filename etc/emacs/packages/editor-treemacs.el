 ;; -*- lexical-binding: t; no-native-compile: t -*-


(use-package treemacs
  :general
  (leader
    "tt" #'treemacs
    "tw" #'treemacs-switch-workspace
    "tC" #'treemacs-create-workspace
    "tE" #'treemacs-edit-workspaces
    "tR" #'treemacs-rename-workspace
    "tp" #'treemacs-add-project-to-workspace)

  ;; Not exposed via autoload by Treemacs
  :commands
  (treemacs-switch-workspace
    treemacs-create-workspace
    treemacs-rename-workspace
    treemacs-add-project-to-workspace)

  :custom
  (treemacs-read-string-input 'from-minibuffer)

  :config
  (use-feature treemacs-evil :demand t)
  (use-feature treemacs-magit :demand t))


(use-package treemacs-evil)


(use-package treemacs-magit)
