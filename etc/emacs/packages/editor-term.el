;; -*- lexical-binding: t -*-

(use-feature eshell
  :commands eshell

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
  :commands multi-term

  :leader
  ("\"" #'multi-term))
