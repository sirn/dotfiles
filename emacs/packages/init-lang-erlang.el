(defun erlang-rebar-locate-root ()
  (locate-dominating-file default-directory "rebar.config"))

(defun erlang-rebar-code-path ()
  (split-string (shell-command-to-string "rebar3 path")))

(defun erlang-rebar-include-path ()
  (split-string (shell-command-to-string "find . -iname include -type d")))

(defun erlang-rebar-hook ()
  (let ((default-directory (erlang-rebar-locate-root)))
    (when default-directory
      (progn
        (let ((paths (erlang-rebar-code-path)))
          (progn
            (setq flycheck-erlang-library-path paths)
            (add-to-list 'inferior-erlang-machine-options "-pa" t)
            (dolist (path paths)
              (add-to-list 'inferior-erlang-machine-options path t))))
        (dolist (path (erlang-rebar-include-path))
(add-to-list 'flycheck-erlang-include-path path t))))))

(req-package erlang
  :mode ("\\.erl\\'" . erlang-mode)
  :interpreter "erl"
  :init
  (progn
    (add-hook 'erlang-mode-hook 'company-mode)
    (add-hook 'erlang-mode-hook 'erlang-rebar-hook))
  :config
  (progn
    (setq erlang-compile-extra-opts '(debug-info))
    (require 'erlang-start)))
