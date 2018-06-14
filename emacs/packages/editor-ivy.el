(use-package counsel
  :after ivy
  :demand t
  :ensure t

  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x C-b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)

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
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "bb" 'ivy-switch-buffer
      "/"  'counsel-rg)))


(use-package counsel-projectile
  :after (counsel projectile)
  :diminish counsel-projectile-mode
  :ensure t

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


(use-package historian
  :defer 1
  :ensure t)


(use-package ivy
  :defer 1
  :diminish ivy-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function ivy-mode nil))

  :config
  (ivy-mode 1))


(use-package ivy-historian
  :after (historian ivy)
  :diminish ivy-historian-mode
  :ensure t

  :preface
  (eval-when-compile
    (declare-function ivy-historian-mode nil))

  :config
  (ivy-historian-mode t))


(use-package swiper
  :after ivy
  :ensure t

  :bind
  (("\C-s" . swiper)))
