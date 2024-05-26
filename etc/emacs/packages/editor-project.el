;; -*- lexical-binding: t; no-native-compile: t -*-

;; --------------------------------------------------------------------------
;;; Project

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
    '((project-dired "Dired")))

  :preface
  (eval-when-compile
    (declare-function gemacs--project-try-local nil)
    (declare-function gemacs--project-ag nil)
    (declare-function gemacs--project-sync nil)
    (declare-function consult-project-buffer nil)
    (declare-function multi-vterm-project nil))

  :config
  (general-with-eval-after-load 'general
    (general-define-key :keymaps 'project-prefix-map "f" #'projectile)

    (with-eval-after-load 'consult
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
  (leader
   "pb" #'consult-project-extra-find
   "pf" #'consult-project-extra-find
   "po" #'consult-project-extra-find-other-window)

  :init
  (with-eval-after-load 'general
    (general-define-key :keymaps 'project-prefix-map "f" #'consult-project-extra-find)
    (add-to-list 'project-switch-commands '(consult-project-extra-find "Find file") t)))


;; --------------------------------------------------------------------------
;;; Workspace

(use-package tabspaces
  :demand t

  :general
  (leader
    "PP" #'tabspaces-switch-or-create-workspace
    "Pp" #'tabspaces-open-or-create-project-and-workspace
    "Pb" #'tabspaces-switch-buffer-and-tab
    "PC" #'tabspaces-clear-buffers
    "Pr" #'tabspaces-remove-selected-buffer
    "PR" #'tabspaces-remove-current-buffer
    "Pk" #'tabspaces-close-workspace
    "PK" #'tabspaces-kill-buffers-close-workspace)

  :preface
  (eval-when-compile
    (declare-function consult--buffer-state nil)
    (declare-function consult--source-buffer nil)
    (declare-function tabspaces-close-workspace nil)
    (declare-function tabspaces-kill-buffers-close-workspace nil)
    (declare-function tabspaces-mode nil)
    (declare-function tabspaces-open-or-create-project-and-workspace nil)
    (declare-function tabspaces-clear-buffers nil)
    (declare-function tabspaces-remove-current-buffer nil)
    (declare-function tabspaces-remove-selected-buffer nil)
    (declare-function tabspaces-switch-buffer-and-tab nil)
    (declare-function tabspaces-switch-or-create-workspace nil)
    (defvar gemacs--tabspaces-consult-source)
    (defvar tabspaces-default-tab)
    (defvar tabspaces-session)
    (defvar tabspaces-remove-to-default)
    (defvar tabspaces-session-auto-restore)
    (defvar tabspaces-use-filtered-buffers-as-default))

  :init
  (setq tabspaces-default-tab "default")
  (setq tabspaces-use-filtered-buffers-as-default t)
  (setq tabspaces-remove-to-default t)
  (setq tabspaces-session t)
  (setq tabspaces-session-auto-restore t)
  (add-hook 'after-init-hook #'tabspaces-mode)

  :config
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (defvar gemacs--tabspaces-consult-source
      (list
       :name "Workspace Buffers"
       :narrow ?w
       :history 'buffer-name-history
       :category 'buffer
       :state #'consult--buffer-state
       :default t
       :items (lambda () (consult--buffer-query
                          :predicate #'tabspaces--local-buffer-p
                          :sort 'visibility
                          :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'gemacs--tabspaces-consult-source)))
