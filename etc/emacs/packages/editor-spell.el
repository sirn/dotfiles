;; -*- lexical-binding: t; no-native-compile: t -*-

;; --------------------------------------------------------------------------
;;; Spell checking

;; Builtin
(use-package ispell
  :custom
  (ispell-program-name (expand-file-name "~/.emacs.d/var/emacs-bin-deps/hunspell"))
  (ispell-really-hunspell t)
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary (no-littering-expand-var-file-name "personal.dict"))
  (ispell-alternate-dictionary (expand-file-name "~/.emacs.d/var/scowl/share/dict/words.txt"))

  :config
  (let ((hunspell-dict-dir (expand-file-name "~/.emacs.d/var/scowl/share/hunspell")))
    (setenv "DICTIONARY" ispell-dictionary)
    (setenv "DICPATH" hunspell-dict-dir)
    (let ((aff-file (expand-file-name "en_US.aff" hunspell-dict-dir)))
      (when (and (file-exists-p aff-file))
        (setq ispell-hunspell-dict-paths-alist `(("en_US" ,aff-file)))
        (ispell-set-spellchecker-params)
        (ispell-hunspell-add-multi-dic "en_US"))))

  :general
  (leader
    "ii" #'ispell
    "iw" #'ispell-word
    "ib" #'ispell-buffer
    "ir" #'ispell-region))


;; Builtin
(use-package flyspell
  :preface
  (eval-when-compile
    (declare-function flyspell-mode nil)
    (declare-function flyspell-prog-mode nil))

  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))
