;; -*- lexical-binding: t; no-native-compile: t -*-

;; --------------------------------------------------------------------------
;;; Project

(use-package project
  :demand t

  :general
  (leader
   "p" '(:keymap project-prefix-map))

  (:keymaps 'project-prefix-map
    "SS" #'gemacs--project-sync
    "f" #'gemacs--project-fd
    "b" #'consult-project-buffer
    "'" #'multi-vterm-project)

  :custom
  (project-vc-extra-root-markers '(".jj" "go.mod"))
  (project-vc-ignores '(".jj"))
  (project-switch-commands '((project-dired "Dired")))

  :preface
  (eval-when-compile
    (declare-function gemacs--project-ag nil)
    (declare-function gemacs--project-fd nil)
    (declare-function gemacs--project-sync nil)
    (declare-function gemacs--project-try-local nil)
    (declare-function consult-project-buffer nil)
    (declare-function multi-vterm-project nil))

  :config
  ;; project-find-file does not read gitignore for non-Git projects
  ;; instead of using project-find-file, we use consult-fd with
  ;; fd utility instead.

  (defun gemacs--project-fd ()
    (interactive)
    (when-let (proj (project-current t))
      (consult-fd (project-root proj))))

  (add-to-list 'project-switch-commands '(gemacs--project-fd "Find file") t)

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


;; --------------------------------------------------------------------------
;;; Workspace

(use-package tabspaces
  :demand t

  :general
  (leader
    "t" '(:keymap tabspaces-command-map))

  :preface
  (eval-when-compile
    (declare-function consult--buffer-state nil)
    (declare-function consult--source-buffer nil)
    (declare-function tabspaces-mode nil)
    (declare-function gemacs--tabspaces-init nil)
    (defvar gemacs--tabspaces-consult-source)
    (defvar tabspaces-default-tab)
    (defvar tabspaces-remove-to-default)
    (defvar tabspaces-use-filtered-buffers-as-default))

  :init
  (setq tabspaces-default-tab "default")
  (setq tabspaces-use-filtered-buffers-as-default t)
  (setq tabspaces-remove-to-default t)

  (defun gemacs--tabspaces-init ()
    ;; Force override; otherwise tabspaces will end up with incomplete
    ;; project-switch-commands (that are defined through customs)
    (require 'project)
    (setq tabspaces-project-switch-commands project-switch-commands)

    (tabspaces-mode +1)
    (tab-bar-rename-tab tabspaces-default-tab))

  (add-hook 'gemacs-after-init-hook #'gemacs--tabspaces-init)

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
