;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package nim-mode
  :preface
  (eval-when-compile
    (declare-function lsp nil)
    (declare-function smie-default-forward-token nil))

  :config
  (el-patch-defun nim-mode-forward-token ()
    "Handle cases where `nim-smie--line-info' is `nil'."
    (when (line-number-at-pos)
      (let ((_pos (point)))
        (skip-chars-forward " \t")
        (forward-comment (point-max))
        (let* ((tok (smie-default-forward-token)))
          (if (< (el-patch-wrap 1 1
                   (or
                     (assoc-default :line nim-smie--line-info)
                     -1))
                 (line-number-at-pos))
              (setq tok ";"))
          tok))))

  (use-feature lsp-mode
    :demand t

    :config
    (add-hook 'nim-mode-hook #'lsp-deferred))

  (use-feature apheleia
    :demand t

    :config
    (add-to-list 'apheleia-formatters '(nimpretty . ("nimpretty" "--out:/dev/stdout" filepath)))
    (add-to-list 'apheleia-mode-alist '(nim-mode . nimpretty))
    (add-hook 'nim-mode-hook #'apheleia-mode)))
