;; -*- lexical-binding: t -*-

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)

  :config
  (ivy-mode +1))


(use-package prescient
  :config
  (prescient-persist-mode +1))


(use-package ivy-prescient
  :demand t

  :preface
  (eval-when-compile
    (declare-function prescient-persist-mode nil))

  :config
  (ivy-prescient-mode +1))


(use-package counsel
  :commands
  (ivy-resume
   counsel-switch-buffer
   counsel-switch-buffer-other-window
   counsel-find-file
   counsel-M-x
   counsel-minibuffer-history
   ivy-backward-kill-word
   ivy-alt-done
   ivy-partial-or-done
   ivy-next-line
   ivy-previous-line
   ivy-alt-done
   ivy-previous-line-or-history
   counsel-up-directory)

  :bind
  (("C-c C-r" . #'ivy-resume)
   ("C-x C-b" . #'counsel-switch-buffer)
   ("C-x C-f" . #'counsel-find-file)
   ("M-x"     . #'counsel-M-x)

   :map minibuffer-local-map
   ("C-r"     . #'counsel-minibuffer-history)

   :map ivy-minibuffer-map
   ("<left>"  . #'ivy-backward-kill-word)
   ("<right>" . #'ivy-alt-done)
   ("C-f"     . #'ivy-partial-or-done)
   ("C-h"     . #'ivy-backward-kill-word)
   ("C-j"     . #'ivy-next-line)
   ("C-k"     . #'ivy-previous-line)
   ("C-l"     . #'ivy-alt-done)
   ("C-r"     . #'ivy-previous-line-or-history)

   :map counsel-find-file-map
   ("<left>"  . #'counsel-up-directory)
   ("C-h"     . #'counsel-up-directory))

  :leader
  ("bb"  #'counsel-switch-buffer
   "wbb" #'counsel-switch-buffer-other-window
   "br"  #'counsel-recentf
   "dv"  #'counsel-describe-variable
   "df"  #'counsel-describe-function
   "/"   #'counsel-ag)

  :config
  (setq counsel-switch-buffer-preview-virtual-buffers nil)
  (setq ivy-initial-inputs-alist nil))


(use-package counsel-projectile
  :commands
  (counsel-projectile-ag
   counsel-projectile-find-file
   counsel-projectile-switch-project
   counsel-projectile-switch-to-buffer)

  :config
  (counsel-projectile-mode +1)

  :leader
  ("p/" #'counsel-projectile-ag
   "pf" #'counsel-projectile-find-file
   "pp" #'counsel-projectile-switch-project
   "pb" #'counsel-projectile-switch-to-buffer))


(use-package ivy-rich
  :demand t

  :init
  (setq ivy-rich-path-style 'abbrev)
  (add-to-list 'ivy-format-functions-alist 'ivy-format-function-line)

  :config
  (ivy-rich-mode +1))


(use-package swiper
  :commands swiper
  :bind
  (("C-s" . #'swiper)))
