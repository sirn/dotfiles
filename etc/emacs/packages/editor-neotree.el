;; -*- lexical-binding: t -*-

(use-package neotree
  :commands neotree-toggle

  :leader
  ("pt" #'gemacs--neotree-project-dir)

  :preface
  (eval-when-compile
    (declare-function neo-global--window-exists-p nil)
    (declare-function gemacs--neotree-project-dir nil))

  :init
  (setq neo-autorefresh nil)

  (use-feature projectile
    :config
    (defun gemacs--neotree-project-dir ()
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)))
          (message "Could not find git project root."))))))
