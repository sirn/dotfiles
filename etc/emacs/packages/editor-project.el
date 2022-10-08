;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package project
  :demand t
  :straight (:host github :repo "emacs-straight/project")

  :general
  (leader
    "pSS" #'gemacs--project-sync
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

  ;; Custom command for syncing projects
  ;;
  ;; Even though project-remember-projects-under exists, it also walks
  ;; down matched project root, which makes detected VC root under
  ;; project root also get added to the list of known projects.
  ;;
  ;; This custom command properly handle such case, and could scan
  ;; ~14,000 directories in less than a second (thanks to `find').

  (defun gemacs--project-sync--codedirs ()
    (seq-map
     #'expand-file-name
     (seq-remove
      #'string-empty-p
      (split-string
       (shell-command-to-string "git config --get-all ghq.root")
       "\n"))))

  (defun gemacs--project-sync--projdirs (codedir)
    (seq-remove
     #'string-empty-p
     (when (file-directory-p codedir)
       (split-string
        (shell-command-to-string
         (mapconcat
          #'shell-quote-argument
          ;; Note: command should match with `gg' in `etc/ksh/kshrc'.
          `("find" ,codedir
            "-not" "-path" "*/node_modules/*"
            "-not" "-path" "*/vendor/*"
            "("
            "-exec" "test" "-d" "{}/.git" ";"
            "-or" "-exec" "test" "-d" "{}/.hg" ";"
            ")"
            "-print" "-prune")
          " "))
        "\n"))))

  (defun gemacs--project-sync ()
    (interactive)
    (project-forget-zombie-projects)
    (project-remember-project (project-current nil "~/.dotfiles"))
    (project-remember-project (project-current nil "~/.dotpriv"))
    (seq-doseq (codedir (gemacs--project-sync--codedirs))
      (seq-doseq (projdir (gemacs--project-sync--projdirs codedir))
        (let* ((dir (abbreviate-file-name projdir))
               (pr (project-current nil dir)))
          (project-remember-project pr))))
    (message "Projects successfully synced")))
