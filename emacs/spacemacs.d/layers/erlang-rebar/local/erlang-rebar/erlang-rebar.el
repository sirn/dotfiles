(defun erlang-rebar-locate-root ()
  (locate-dominating-file default-directory "rebar.config"))

(defun erlang-rebar-code-path ()
  (let ((default-directory (erlang-rebar-locate-root)))
    (when default-directory
      ;; TODO: Unfortunately broke if path contains space as rebar do not
      ;; escape the path and we do not handle path escaping either.
      (split-string (shell-command-to-string "rebar3 path")))))

(defun erlang-rebar-hook ()
  (setq flycheck-erlang-include-path
        (list "../include/" "../../include/" "../../"))
  (let ((paths (erlang-rebar-code-path)))
    (progn
      (setq flycheck-erlang-library-path paths)
      (add-to-list 'inferior-erlang-machine-options "-pa" t)
      (dolist (path paths)
        (add-to-list 'inferior-erlang-machine-options path t)))))

(provide 'erlang-rebar)
