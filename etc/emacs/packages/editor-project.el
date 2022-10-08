;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package project
  :demand t
  :straight (:host github :repo "emacs-straight/project")

  :general
  (leader
    "pSR" #'gemacs--project-sync
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
    (declare-function gemacs--project-sync nil)
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
      (leader "p/" #'gemacs--project-ag)))

  (defun gemacs--project-sync ()
    (interactive)
    (project-forget-zombie-projects)
    (let* ((codedirs
            (seq-remove #'string-empty-p
                          (split-string
                           (shell-command-to-string "git config --get-all ghq.root")
                           "\n")))
           (projdirs
            (seq-remove
             #'string-empty-p
             (flatten-tree
              (seq-map
               (lambda (codedir)
                 (let ((realcodedir (expand-file-name codedir)))
                   (when (file-directory-p realcodedir)
                     (split-string
                      (shell-command-to-string
                       (mapconcat
                        #'shell-quote-argument
                        ;; Note: command should match with `gg' in `etc/ksh/kshrc'.
                        `("find" ,realcodedir
                          "-not" "-path" "*/node_modules/*"
                          "-not" "-path" "*/vendor/*"
                          "("
                          "-exec" "test" "-d" "{}/.git" ";"
                          "-or" "-exec" "test" "-d" "{}/.hg" ";"
                          ")"
                          "-print" "-prune")
                        " "))
                      "\n"))))
               codedirs)))))
      (seq-doseq (projdir projdirs)
        (let* ((dir (abbreviate-file-name projdir))
               (pr (project-current nil dir)))
          (project-remember-project pr t))))))
