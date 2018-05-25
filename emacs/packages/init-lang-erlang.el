(defun custom/erlang-rebar-locate-root ()
  (locate-dominating-file default-directory "rebar.config"))

(defun custom/erlang-rebar-code-path ()
  (split-string (shell-command-to-string "rebar3 path -s :") ":"))

(defun custom/erlang-rebar-include-path ()
  (split-string (shell-command-to-string "find . -iname include -type d")))

(defun custom/erlang-rebar-hook ()
  (let ((default-directory (custom/erlang-rebar-locate-root)))
    (when default-directory
      (progn
        (let ((paths (custom/erlang-rebar-code-path)))
          (progn
            (setq flycheck-erlang-library-path paths)
            (add-to-list 'inferior-erlang-machine-options "-pa" t)
            (dolist (path paths)
              (add-to-list 'inferior-erlang-machine-options path t))))
        (dolist (path (custom/erlang-rebar-include-path))
          (add-to-list 'flycheck-erlang-include-path path t))))))

(req-package erlang
  :require company
  :mode ("\\.erl\\'" . erlang-mode)
  :interpreter "erl"
  :init
  (add-hook 'erlang-mode-hook 'custom/erlang-rebar-hook)
  :config
  (progn
    (setq erlang-compile-extra-opts '(debug-info))
    (require 'erlang-start)))
