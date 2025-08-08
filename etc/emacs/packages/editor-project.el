;; -*- lexical-binding: t; no-native-compile: t -*-

;; --------------------------------------------------------------------------
;;; Project

(use-package project
  :demand t

  :general
  ("C-x C-f" #'project-find-file)
  (leader
   "p" '(:keymap project-prefix-map))

  (:keymaps 'project-prefix-map
    "'" #'eat-project
    "a" #'gemacs--project-aidermacs-run
    "b" #'consult-project-buffer
    "c" #'gemacs--project-claude-code-ide
    "C" #'gemacs--project-claude-code-ide-continue
    "d" #'project-dired
    "f" #'gemacs--project-fd
    "g" #'gemacs--project-gptel
    "m" #'magit-project-status
    "p" #'project-switch-project
    "s" #'consult-ripgrep
    "S" #'gemacs--project-sync)

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
    (declare-function consult-ripgrep nil)
    (declare-function magit-project-status nil)
    (declare-function eat-project nil)
    (declare-function project-dired nil)
    (declare-function project-read-project-name nil)
    (declare-function project-switch-project nil)
    (declare-function gemacs--project-switch-command nil)
    (declare-function gemacs--project-switch-transient-menu nil)
    (declare-function gemacs--project-switch-smuggle-chosen-project nil)
    (declare-function gemacs--project-claude-code-ide nil)
    (declare-function gemacs--project-claude-code-ide-continue nil)
    (declare-function gemacs--project-aidermacs-run nil)
    (declare-function gemacs--project-gptel nil))

  :config
  (require 'transient)

  ;; https://codeberg.org/woolsweater/.emacs.d/src/commit/f7d1adab1784627903a7c44e19c3bc91c877283b/startup/builtin.el
  (defun gemacs--project-switch-smuggle-chosen-project (&rest _args)
    "Ensure project-current-directory-override persists across transient invocations."
    (when project-current-directory-override
      (let ((pre (intern "gemacs--project-switch--pre-command-hook"))
            (on-exit (intern "gemacs--project-switch--transient-exit-hook"))
            (orig-buffer (current-buffer))
            (proj project-current-directory-override))
        (fset pre
              (lambda ()
                (with-current-buffer orig-buffer
                  (setq-local project-current-directory-override proj))))
        (fset on-exit
              (lambda ()
                (with-current-buffer orig-buffer
                  (kill-local-variable 'project-current-directory-override))
                (remove-hook 'transient-exit-hook on-exit)
                (remove-hook 'pre-command-hook pre)))
        (add-hook 'pre-command-hook pre)
        (add-hook 'transient-exit-hook on-exit))))

  (transient-define-prefix gemacs--project-switch-transient-menu ()
    "Project commands"
    ["Find & Search"
     ("b" "Buffers" consult-project-buffer)
     ("d" "Dired" project-dired)
     ("f" "Find file (fd)" gemacs--project-fd)
     ("F" "Find file" project-find-file)
     ("s" "Ripgrep" consult-ripgrep)]
    ["Coding Assistant"
     ("a" "Aidermacs" gemacs--project-aidermacs-run)
     ("c" "Claude Code" gemacs--project-claude-code-ide)
     ("C" "Claude Code (continue)" gemacs--project-claude-code-ide-continue)
     ("g" "GPTel" gemacs--project-gptel)]
    ["Tools"
     ("m" "Magit" magit-project-status)
     ("'" "Terminal (Eat)" eat-project)]
    ["Maintenance"
     ("S" "Sync projects" gemacs--project-sync)])

  (defun gemacs--project-switch-command ()
    (interactive)
    (gemacs--project-switch-smuggle-chosen-project)
    (gemacs--project-switch-transient-menu))

  (setq project-switch-commands #'gemacs--project-switch-command)

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
    (message "Projects successfully synced"))

  (defun gemacs--project-claude-code-ide ()
    "Open claude-code-ide with project root as default directory."
    (interactive)
    (require 'claude-code-ide)
    (when-let ((project (project-current t)))
      (let ((default-directory (project-root project)))
        (claude-code-ide))))

  (defun gemacs--project-claude-code-ide-continue ()
    "Continue claude-code-ide with project root as default directory."
    (interactive)
    (require 'claude-code-ide)
    (when-let ((project (project-current t)))
      (let ((default-directory (project-root project)))
        (claude-code-ide-continue))))

  (defun gemacs--project-aidermacs-run ()
    "Run aidermacs-run with project root as default directory."
    (interactive)
    (if (fboundp 'aidermacs-run)
        (when-let ((project (project-current t)))
          (let ((default-directory (project-root project)))
            (aidermacs-run)))
      (error "Aidermacs is not enabled")))

  (defun gemacs--project-gptel ()
    "Open gptel with project root as default directory."
    (interactive)
    (if (fboundp 'gptel)
        (when-let ((project (project-current t)))
          (let ((default-directory (project-root project)))
            (gptel)))
      (error "GPTel is not enabled"))))
