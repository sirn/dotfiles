;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package yasnippet
  :demand t

  :general
  (leader
   "e y s" #'yas-insert-snippet
   "e y n" #'yas-new-snippet
   "e y v" #'yas-visit-snippet-file)

  :preface
  (eval-when-compile
    (declare-function yas-global-mode nil)
    (declare-function yas-reload-all nil))

  :custom
  (yas-snippet-dirs
   `(,(expand-file-name "~/.dotfiles/etc/emacs/snippets")
     ,(expand-file-name "~/.local/share/emacs/snippets")
     ,(expand-file-name "~/.local/share/yasnippets")))

  :config
  (yas-reload-all)
  (yas-global-mode +1))


(use-package yasnippet-snippets
  :after yasnippet
  :demand t)
