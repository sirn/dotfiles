(use-package smart-mode-line
  :after (git-gutter s) ;; git-gutter is weird
  :straight t

  :preface
  (eval-when-compile
    (declare-function gr/insert-mode-line nil)
    (declare-function sml/setup nil)
    (defvar sml/theme))

  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme nil)

  :config
  (sml/setup)

  ;; Custom mode lines
  ;;
  ;; If two mode-line shares the same position number, the one that defined last
  ;; will come first. The gr/insert-mode-line function is taken from winum-mode.

  (defgroup gr-mode-line-faces '()
    "Gridth's faces customization for mode lines."
    :group 'faces
    :group 'gr
    :prefix 'gr)

  (defun gr/insert-mode-line (new-mode-line &optional position)
    (let ((mode-line (default-value 'mode-line-format))
          (res))
      (dotimes (i (or position (length mode-line)))
        (push (pop mode-line) res))
      (push new-mode-line res)
      (while mode-line
        (push (pop mode-line) res))
      (setq-default mode-line-format (nreverse res)))
    (force-mode-line-update t))

  ;; Mode-Line for evil-mode

  (defface gr/mode-line-evil '((t :inherit sml/global)) "" :group 'gr-mode-line-faces)

  (defun gr/mode-line-evil ()
    (when (boundp 'evil-state)
      (propertize (format "%8s "
                          (cond ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
                                ((eq evil-visual-selection 'block) "V-BLOCK")
                                ((eq evil-visual-selection 'line) "V-LINE")
                                (t "VISUAL")))
                  'face 'gr/mode-line-evil
                  'help-echo "Evil mode")))

  (gr/insert-mode-line '(:eval (gr/mode-line-evil)) 1)

  ;; Mode-Line for winum

  (defface gr/mode-line-winum '((t :inherit sml/global)) "" :group 'gr-mode-line-winum)

  (defun gr/mode-line-winum ()
    (if (fboundp 'winum-get-number-string)
        (let ((str (winum-get-number-string)))
          (propertize (format "%2s " (if (eq str "") "1" str))
                      'face 'gr/mode-line-winum
                      'help-echo "Window number"))))

  (gr/insert-mode-line '(:eval (gr/mode-line-winum)) 1))
