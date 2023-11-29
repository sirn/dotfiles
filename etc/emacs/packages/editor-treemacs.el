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

  :preface
  (eval-when-compile
    (declare-function treemacs-hide-gitignored-files-mode nil)
    (defvar treemacs-read-string-input))

  ;; Not exposed via autoload by Treemacs
  :commands
  (treemacs-switch-workspace
    treemacs-create-workspace
    treemacs-rename-workspace
    treemacs-add-project-to-workspace
    treemacs-hide-gitignored-files-mode)

  :custom
  (treemacs-read-string-input 'from-minibuffer)

  :config
  (require 'treemacs-evil)
  (require 'treemacs-magit)
  (treemacs-hide-gitignored-files-mode +1))


(use-package treemacs-evil)


(use-package treemacs-magit)
