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
    (defun neotree-project-dir ()
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
        "pt" 'neotree-project-dir)))

  :config
  (evil-set-initial-state 'neotree-mode 'normal)
  (evil-define-key 'normal neotree-mode-map
    (kbd "TAB")  'neotree-stretch-toggle
    (kbd "RET")  'neotree-enter
    (kbd "|")    'neotree-enter-vertical-split
    (kbd "-")    'neotree-enter-horizontal-split
    (kbd "'")    'neotree-quick-look
    (kbd "c")    'neotree-create-node
    (kbd "C")    'neotree-copy-node
    (kbd "d")    'neotree-delete-node
    (kbd "g")    'neotree-refresh
    (kbd "H")    'neotree-select-previous-sibling-node
    (kbd "j")    'neotree-next-line
    (kbd "J")    'neotree-select-down-node
    (kbd "k")    'neotree-previous-line
    (kbd "K")    'neotree-select-up-node
    (kbd "L")    'neotree-select-next-sibling-node
    (kbd "q")    'neotree-hide
    (kbd "r")    'neotree-rename-node
    (kbd "R")    'neotree-change-root
    (kbd "s")    'neotree-hidden-file-toggle))
