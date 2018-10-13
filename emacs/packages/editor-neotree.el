(use-package neotree
  :commands neotree-toggle
  :straight t

  :preface
  (eval-when-compile
    (declare-function neo-global--window-exists-p nil)
    (declare-function neotree-dir nil)
    (declare-function neotree-find nil))

  :init
  (setq neo-autorefresh nil)

  (with-eval-after-load 'projectile
    (defun gr/neotree-project-dir ()
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)))
          (message "Could not find git project root."))))

    (with-eval-after-load 'evil-leader
      (evil-leader/set-key
        "pt" 'gr/neotree-project-dir))))
