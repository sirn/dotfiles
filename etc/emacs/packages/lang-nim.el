;; -*- lexical-binding: t; no-native-compile: t -*-

(use-package nim-mode
  :preface
  (eval-when-compile
    (declare-function apheleia-mode nil)
    (declare-function smie-default-forward-token nil))

  :hook
  ((nim-mode . apheleia-mode)
   (nim-mode . eglot-ensure)
   (nim-mode . flymake-mode))

  :config
  ;; Upstream nim-mode doesn't guard against nil nim-smie--line-info,
  ;; causing errors when (< nil integer) is evaluated. This patch adds
  ;; a fallback value of -1 when the alist lookup returns nil.
  ;; TODO: Submit fix upstream to nim-lang/nim-mode.
  (el-patch-defun nim-mode-forward-token ()
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

  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(nimpretty . ("nimpretty" "--out:/dev/stdout" filepath)))
    (add-to-list 'apheleia-mode-alist '(nim-mode . nimpretty)))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(nim-mode . ("nimlsp")))))
