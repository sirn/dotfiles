(defconst emacs-start-time (current-time))


;; Use custom file at alternate path
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))


;; Private configurations
(let ((private-init-file "~/.dotpriv/emacs/init.el"))
  (if (file-exists-p private-init-file)
      (load private-init-file)))


;; SSL cert
(eval-when-compile
  (defvar gnutls-trustfiles))

(with-eval-after-load 'gnutls
  (let ((cert "/usr/local/etc/libressl/cert.pem"))
    (when (file-exists-p cert)
      (add-to-list 'gnutls-trustfiles cert))))


;; Remove banners
(setq inhibit-startup-message t)


;; Disable backups
(setq backup-inhibited t)
(setq auto-save-default nil)


;; Initialize straight.el
(setq straight-recipes-gnu-elpa-use-mirror t)
(let ((bootstrap-version 4)
      (bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-process-buffer " *straight-process*")
(straight-use-package 'diminish)
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package)
  (setq use-package-compute-statistics t)
  (setq use-package-expand-minimally byte-compile-current-file))


;; Packages
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
