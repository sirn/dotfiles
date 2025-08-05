;; -*- lexical-binding: t; no-native-compile: t -*-

;; --------------------------------------------------------------------------
;;; Project

(use-package project
  :demand t

  :general
  ("C-x C-f" #'project-find-file)
  (leader
   "SPC p" '(:keymap project-prefix-map))

  (:keymaps 'project-prefix-map
    "'" #'multi-vterm-project
    "b" #'consult-project-buffer
    "d" #'project-dired
    "f" #'gemacs--project-fd
    "g" #'consult-grep
    "m" #'magit-project-status
    "r" #'consult-ripgrep
    "s" #'gemacs--project-sync)

  :custom
  (project-vc-extra-root-markers '(".jj" "go.mod"))
  (project-vc-ignores '(".jj"))

  :preface
  (eval-when-compile
    (require 'transient)
    (declare-function gemacs--project-ag nil)
    (declare-function gemacs--project-fd nil)
    (declare-function gemacs--project-sync nil)
    (declare-function gemacs--project-try-local nil)
    (declare-function consult-project-buffer nil)
    (declare-function consult-grep nil)
    (declare-function consult-ripgrep nil)
    (declare-function magit-project-status nil)
    (declare-function multi-vterm-project nil)
    (declare-function project-dired nil)
    (declare-function project-read-project-name nil)
    (declare-function project-switch-project nil)
    (declare-function gemacs--override-project-switch-project nil)
    (declare-function gemacs--project-switch-command nil)
    (declare-function gemacs--project-switch-transient-menu nil))

  :config
  (setq
    project-switch-commands
    '((gemacs--project-fd "Find file (fd)" "f")
      (consult-project-buffer "Project buffer" "b")
      (project-dired "Dired" "d")
      (consult-grep "Grep" "g")
      (consult-ripgrep "Ripgrep" "r")
      (magit-project-status "Magit status" "m")
      (multi-vterm-project "VTerm" "'")
      (gemacs--project-sync "Sync projects" "s")))

  ;; project-find-file does not read gitignore for non-Git projects
  ;; instead of using project-find-file, we use consult-fd with
  ;; fd utility instead.

  (defun gemacs--project-fd ()
    (interactive)
    (when-let (proj (project-current t))
      (consult-fd (project-root proj))))


  ;; For custom projects without requiring .git
  ;; https://christiantietze.de/posts/2022/03/mark-local-project.el-directories/

  (defun gemacs--project-try-local (dir)
    "Checks if DIR is a Jujutsu or non-VC project."
    (catch 'ret
      (let ((markers '(".project" ".projectile")))
        (dolist (f markers)
          (when-let ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'local root)))))))

  (cl-defmethod project-root ((project (head local)))
    (cdr project))

  (add-to-list 'project-find-functions #'gemacs--project-try-local)

  (defun gemacs--project-sync--projdirs ()
    (split-string
     (s-trim
      (shell-command-to-string
       (concat (getenv "HOME") "/.dotfiles/bin/pom list -u")))
     "\n"))

  (defun gemacs--project-sync ()
    (interactive)
    (project-forget-zombie-projects)
    (project-remember-project (project-current nil "~/.dotfiles"))
    (seq-doseq (projdir (gemacs--project-sync--projdirs))
      (let* ((dir (abbreviate-file-name projdir))
             (pr (project-current nil dir)))
        (project-remember-project pr)))
    (message "Projects successfully synced")))
