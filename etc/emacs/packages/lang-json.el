;; -*- lexical-binding: t -*-

(use-package json-mode
  :init/el-patch
  (defconst json-mode-standard-file-ext '(".json" ".jsonld")
    "List of JSON file extensions.")

  (defsubst json-mode--update-auto-mode (filenames)
    "Update the `json-mode' entry of `auto-mode-alist'.
FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
    (let* ((new-regexp
            (rx-to-string
             `(seq (eval
                    (cons 'or
                          (append json-mode-standard-file-ext
                                  ',filenames)))
                   eot)))
           (new-entry (cons new-regexp 'json-mode))
           (old-entry (when (boundp 'json-mode--auto-mode-entry)
                        json-mode--auto-mode-entry)))
      (setq auto-mode-alist (delete old-entry auto-mode-alist))
      (add-to-list 'auto-mode-alist new-entry)
      new-entry))

  (defcustom json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock")
    "List of filename as string to pass for the JSON entry of
`auto-mode-alist'. Note however that custom `json-mode' entries
in `auto-mode-alist' won’t be affected."
    :group 'json-mode
    :type '(repeat string)
    :set (lambda (symbol value)
           "Update SYMBOL with a new regexp made from VALUE.
This function calls `json-mode--update-auto-mode' to change the
`json-mode--auto-mode-entry' entry in `auto-mode-alist'."
           (set-default symbol value)
           (setq json-mode--auto-mode-entry
                 (json-mode--update-auto-mode value))))

  (defvar json-mode--auto-mode-entry
    (json-mode--update-auto-mode json-mode-auto-mode-list)
    "Regexp generated from the `json-mode-auto-mode-list'.")

  :config
  (use-feature flycheck
    :config
    (gemacs-protect-macros
      (cl-pushnew
       (cons
        (flycheck-rx-to-string
         `(and
           line-start
           (message "No JSON object could be decoded")
           line-end)
         'no-group)
        'error)
       (flycheck-checker-get 'json-python-json 'error-patterns)
       :test #'equal))))


(use-package jq-mode
  :demand t)
