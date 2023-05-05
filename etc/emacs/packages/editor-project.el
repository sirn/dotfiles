;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package project
  :demand t

  :straight (:host github :repo "emacs-straight/project")

  :general
  (leader
    "pSS" #'gemacs--project-sync
    "pp" #'project-switch-project
    "pk" #'project-kill-buffers
    "p'" #'project-eshell
    "p!" #'project-async-shell-command
    "pc" #'project-compile)

  :preface
  (eval-when-compile
    (declare-function gemacs--project-try-local nil)
    (declare-function gemacs--project-ag nil)
    (declare-function gemacs--project-sync nil))

  :config
  (use-feature consult
    :general
    ("C-x p b" #'consult-project-buffer)

    (leader
      "pb" #'consult-project-buffer))

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
            "-or" "-exec" "test" "-f" "{}/.project" ";"
            "-or" "-exec" "test" "-f" "{}/.projectile" ";"
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


(use-package consult-project-extra
  :demand t

  :general
  ("C-c p f" #'consult-project-extra-find
   "C-c p o" #'consult-project-extra-find-other-window)

  (leader
   "pf" #'consult-project-extra-find
   "po" #'consult-project-extra-find-other-window))
