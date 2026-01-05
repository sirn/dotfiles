;; -*- lexical-binding: t; no-native-compile: t -*-

;; --------------------------------------------------------------------------
;;; Spell checking

;; Builtin: spell checking interface
(use-package ispell
  :custom
  (ispell-program-name (expand-file-name "~/.emacs.d/var/emacs-bin-deps/hunspell"))
  (ispell-really-hunspell t)
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary (no-littering-expand-var-file-name "personal.dict"))
  (ispell-alternate-dictionary (expand-file-name "~/.emacs.d/var/scowl/share/dict/words.txt"))

  :config
  (setenv "DICTIONARY" ispell-dictionary)
  (let ((hunspell-dict-dir (expand-file-name "~/.emacs.d/var/scowl/share/hunspell")))
    (setenv "DICPATH" hunspell-dict-dir))

  :general
  (leader
    "e i i" #'ispell
    "e i w" #'ispell-word
    "e i b" #'ispell-buffer
    "e i r" #'ispell-region))
