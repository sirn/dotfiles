{ config, lib, pkgs, ... }:

let
  emacsPackage = pkgs.emacsPackagesFor config.programs.emacs.package;

  emacsWithPackages = emacsPackage.emacsWithPackages (epkgs: with epkgs; [
    evil
    evil-collection
    general
    general
    magit
    modus-themes
    no-littering
    prescient
    project
    telephone-line
    vertico
    vertico-prescient

    pkgs.git
    pkgs.gitAndTools.git-lfs
    pkgs.gitAndTools.git-crypt
  ]);

  magitInit = pkgs.writeTextDir "share/emacs/site-lisp/magit-init.el" ''
    (setq package-enable-at-startup nil)
    (setq native-comp-async-report-warnings-errors nil)
    (setq user-emacs-directory (format "~/.emacs.d/magit/%s/" emacs-version))
    (setq custom-file (concat user-emacs-directory "custom.el"))

    (require 'general)
    (general-create-definer leader-def
      :keymaps 'override
      :states '(normal visual motion insert)
      :prefix "SPC"
      :non-normal-prefix "M-SPC")

    (setq make-backup-files nil)
    (setq auto-save-default nil)
    (setq create-lockfiles nil)
    (setq inhibit-startup-screen t)
    (setq initial-scratch-message nil)
    (defalias 'yes-or-no-p 'y-or-n-p)

    (require 'magit)
    (leader-def "gs" #'magit-project-status)
    (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
    (setq magit-bind-magit-project-status nil)
    (setq magit-remote-set-if-missing t)
    (defun setup-standalone-magit ()
      (magit-project-status)
      (delete-other-windows)
      (with-eval-after-load 'evil
        (evil-local-set-key 'normal "q" #'save-buffers-kill-emacs)))
    (add-hook 'after-init-hook 'setup-standalone-magit)

    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (require 'evil)
    (require 'evil-collection)
    (evil-collection-init 'magit)
    (evil-mode +1)

    (require 'project)
    (require 'vertico)
    (vertico-mode +1)

    (require 'prescient)
    (require 'vertico-prescient)
    (setq prescient-history-length 1000)
    (setq completion-styles '(prescient basic))
    (prescient-persist-mode +1)
    (vertico-prescient-mode +1)

    (leader-def
      "wo"  #'other-window
      "wd"  #'delete-window
      "wD"  #'delete-other-windows
      "w-"  #'split-window-below
      "w/"  #'split-window-right
      "w="  #'balance-windows
      "bd"  #'kill-buffer
      "bD"  #'kill-buffer-and-window
      "bb"  #'switch-to-buffer
      "wbb" #'switch-to-buffer-other-window)

    (tool-bar-mode -1)
    (menu-bar-mode -1)

    (require 'telephone-line)
    (defun setup-modeline ()
      (telephone-line-mode +1))
    (add-hook 'after-init-hook 'setup-modeline)

    (require 'modus-themes)
    (defun setup-ui ()
      (setq modus-themes-mode-line '(borderless))
      (load-theme 'modus-vivendi t))
    (add-hook 'after-init-hook 'setup-ui)

    (run-hooks 'after-init-hook)
  '';

  magitWrapped = pkgs.writeScriptBin "magit" ''
    #!${pkgs.bash}/bin/bash
    if [ "$(git rev-parse --is-inside-work-tree)" = "true" ]; then
        exec ${emacsWithPackages}/bin/emacs -q --no-splash -nw \
            -l "${emacsWithPackages.deps}/share/emacs/site-lisp/site-start.el" \
            -l "${magitInit}/share/emacs/site-lisp/magit-init.el"
    fi
  '';
in
{
  home.packages = [ magitWrapped ];
}
