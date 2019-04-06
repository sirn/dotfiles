;; Note: need to be included before counsel/swiper (same repo)
(use-package ivy
  :diminish ivy-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function ivy-mode nil))

  :init
  (setq ivy-use-virtual-buffers t)

  :config
  (ivy-mode t))


(use-package counsel
  :after ivy
  :demand t
  :straight t

  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x C-b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("M-x"     . counsel-M-x)

   :map minibuffer-local-map
   ("C-r"     . counsel-minibuffer-history)

   :map ivy-minibuffer-map
   ("<left>"  . ivy-backward-kill-word)
   ("<right>" . ivy-alt-done)
   ("C-f"     . ivy-partial-or-done)
   ("C-h"     . ivy-backward-kill-word)
   ("C-j"     . ivy-next-line)
   ("C-k"     . ivy-previous-line)
   ("C-l"     . ivy-alt-done)
   ("C-r"     . ivy-previous-line-or-history)

   :map counsel-find-file-map
   ("<left>"  . counsel-up-directory)
   ("C-h"     . counsel-up-directory))

  :init
  (add-to-list 'ivy-ignore-buffers "^#")
  (add-to-list 'ivy-ignore-buffers "^\\*irc\\-")

  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "bb" 'ivy-switch-buffer
      "br" 'counsel-recentf
      "dv" 'counsel-describe-variable
      "df" 'counsel-describe-function
      "/"  'counsel-rg)))


(use-package counsel-projectile
  :after (counsel projectile)
  :diminish counsel-projectile-mode
  :straight t

  :preface
  (eval-when-compile
    (declare-function counsel-projectile-mode nil))

  :commands
  (counsel-projectile-rg
   counsel-projectile-find-file
   counsel-projectile-switch-project
   counsel-projectile-switch-to-buffer)

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "p/" 'counsel-projectile-rg
      "pf" 'counsel-projectile-find-file
      "pp" 'counsel-projectile-switch-project
      "pb" 'counsel-projectile-switch-to-buffer))

  :config
  (counsel-projectile-mode t))


(use-package ivy-prescient
  :after (ivy prescient)
  :straight t

  :preface
  (eval-when-compile
    (declare-function ivy-prescient-mode nil))

  :config
  (ivy-prescient-mode t))


(use-package ivy-rich
  :straight t
  :after (ivy counsel)

  :preface
  (eval-when-compile
    (defvar ivy-rich-path-style)
    (declare-function ivy-rich-mode nil))

  :init
  (setq ivy-rich-path-style 'abbrev)

  :config
  (ivy-rich-mode t))


(use-package prescient
  :straight t

  :preface
  (eval-when-compile
    (declare-function prescient-persist-mode nil))

  :config
  (prescient-persist-mode t))


(use-package swiper
  :after ivy
  :straight t
  :bind
  (("\C-s" . swiper)))
