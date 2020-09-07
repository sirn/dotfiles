;; -*- lexical-binding: t -*-

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")

  :preface
  (eval-when-compile
    (declare-function selectrum-mode nil)
    (declare-function gemacs--selectrum-insert-or-submit-current-candidate nil))

  :bind
  (("C-x C-b" . #'switch-to-buffer)

   :map selectrum-minibuffer-map
   ("<left>"  . #'backward-kill-word)
   ("<right>" . #'gemacs--selectrum-insert-or-submit-current-candidate)
   ("TAB"     . #'gemacs--selectrum-insert-or-submit-current-candidate)
   ("C-f"     . #'gemacs--selectrum-insert-or-submit-current-candidate)
   ("C-l"     . #'gemacs--selectrum-insert-or-submit-current-candidate)
   ("C-h"     . #'backward-kill-word)
   ("C-j"     . #'selectrum-next-candidate)
   ("C-k"     . #'selectrum-previous-candidate)
   ("C-r"     . #'selectrum-previous-candidate)
   ("C-M-j"   . #'selectrum-submit-exact-input))

  :leader
  ("bb"  #'switch-to-buffer
   "wbb" #'switch-to-buffer-other-window
   "dv"  #'describe-variable
   "df"  #'describe-function)

  :init
  (defun gemacs--selectrum-insert-or-submit-current-candidate ()
    "Insert current candidate depending, or forward to
`selectrum-select-current-candidate' if input text hasn't changed since
last completion

Similar to ivy's `ivy-partial-or-done'."
    (interactive)
    (progn
      (let ((prev-input (selectrum-get-current-input)))
        (when (> (length (selectrum-get-current-candidates)) 0)
          (selectrum-insert-current-candidate))
        (when (string= prev-input (selectrum-get-current-input))
          (selectrum-select-current-candidate)))))

  (selectrum-mode +1))


(use-package prescient
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000))


(use-package selectrum-prescient
  :straight (:host github
              :repo "raxod502/prescient.el"
              :files ("selectrum-prescient.el"))

  :demand t

  :preface
  (eval-when-compile
    (declare-function selectrum-prescient-mode nil))

  :after selectrum
  :config
  (selectrum-prescient-mode +1))
