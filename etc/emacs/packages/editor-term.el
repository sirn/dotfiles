;; -*- lexical-binding: t; no-native-compile: t -*-

(use-feature eshell
  :preface
  (eval-when-compile
    (declare-function gemacs--eshell-remove-pcomplete nil))

  :leader
  ("'" #'eshell)

  :config
  (defun gemacs--eshell-remove-pcomplete ()
    "Remove deprecated company hook from eshell."
    (remove-hook
      'completion-at-point-functions
      #'pcomplete-completions-at-point t))

  (add-hook 'eshell-mode-hook #'gemacs--eshell-remove-pcomplete))


(use-package multi-term
  :leader
  ("\"" #'multi-term))
