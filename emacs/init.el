(defconst emacs-start-time (current-time))


;; Use custom file at alternate path
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))


;; Remove banners
(setq inhibit-startup-message t)


;; Disable backups
(setq backup-inhibited t)
(setq auto-save-default nil)


;; Initialize package.el with custom repositories.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archive-priorities '("org" . 20))
(add-to-list 'package-archive-priorities '("melpa" . 10))
(add-to-list 'package-archive-priorities '("gnu" . 0))
(package-initialize)


;; Use req-package to make .emacs manageable.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'diminish)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-expand-minimally byte-compile-current-file))
(require 'diminish)
(require 'bind-key)


;; Packages
(setq use-package-always-ensure t)
(let ((loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history)))))
  (dolist (file (directory-files "~/.dotfiles/emacs/packages" t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library loaded)
        (load library nil t)
        (push library loaded)))))


;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)


;; Benchmarking
(add-hook
 'after-init-hook
 `(lambda ()
    (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
      (message "Loading %s...done (%.3fs)" ,load-file-name elapsed t))))
