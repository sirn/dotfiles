;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package project
  :demand t
  :straight (:host github :repo "emacs-straight/project")

  :general
  (leader
    "pf" #'project-find-file
    "pp" #'project-switch-project
    "pb" #'project-switch-to-buffer
    "pk" #'project-kill-buffers
    "p'" #'project-eshell
    "p!" #'project-async-shell-command
    "pc" #'project-compile)

  :preface
  (eval-when-compile
    (declare-function ag-project-regexp nil)
    (declare-function gemacs--project-ag nil)
    (declare-function leader nil)
    (declare-function project-async-shell-command nil)
    (declare-function project-compile nil)
    (declare-function project-eshell nil)
    (declare-function project-find-file nil)
    (declare-function project-kill-buffers nil)
    (declare-function project-switch-project nil))

  :config
  (use-feature ag
    :preface
    :init
    (defun gemacs--project-ag ()
      (interactive)
      (let* ((pr (project-current t))
             (default-directory (project-root pr)))
        (call-interactively 'ag-project-regexp)))

    (general-with-eval-after-load 'general
      (leader "p/" #'gemacs--project-ag))))
