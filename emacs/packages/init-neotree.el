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

(req-package neotree
  :require (evil-leader projectile)
  :commands (neotree-toggle neotree-dir neotree-find)
  :init
  (evil-leader/set-key
    "pt" 'neotree-project-dir)
  :config
  (progn
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
      (kbd "s")    'neotree-hidden-file-toggle)))
