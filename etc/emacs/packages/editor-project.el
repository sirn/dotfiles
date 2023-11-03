;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package project
  :demand t

  :general
  (leader
    "pSS" #'gemacs--project-sync
    "pp" #'project-switch-project
    "pk" #'project-kill-buffers
    "p!" #'project-async-shell-command
    "pc" #'project-compile)

  :custom
  (project-switch-commands
    '((project-find-file "Find file")
      (project-dired "Dired")))

  :preface
  (eval-when-compile
    (declare-function gemacs--project-try-local nil)
    (declare-function gemacs--project-ag nil)
    (declare-function gemacs--project-sync nil)
    (declare-function consult-project-buffer nil)
    (declare-function multi-vterm-project nil))

  :config
  (general-with-eval-after-load 'general
    (with-eval-after-load 'consult
      (general-define-key "C-x p b" #'consult-project-buffer)
      (leader "pb" #'consult-project-buffer))

    (with-eval-after-load 'multi-vterm
      (leader "p'" #'multi-vterm-project)))

  ;; For custom projects without requiring .git
  ;; https://christiantietze.de/posts/2022/03/mark-local-project.el-directories/

  (defun gemacs--project-try-local (dir)
    "Checks if DIR is a non-VC project."
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
    (project-remember-project (project-current nil "~/.dotpriv"))
    (seq-doseq (projdir (gemacs--project-sync--projdirs))
      (let* ((dir (abbreviate-file-name projdir))
             (pr (project-current nil dir)))
        (project-remember-project pr)))
    (message "Projects successfully synced")))


(use-package consult-project-extra
  :demand t

  :general
  ("C-c p f" #'consult-project-extra-find
   "C-c p o" #'consult-project-extra-find-other-window)

  (leader
   "pf" #'consult-project-extra-find
   "po" #'consult-project-extra-find-other-window))
