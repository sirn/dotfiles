
(use-package erlang
  :interpreter "erl"
  :mode ("\\.erl\\'" . erlang-mode)
  :straight t

  :preface
  (eval-when-compile
    (defvar inferior-erlang-machine-options)
    (declare-function gr/erlang-rebar-locate-root nil)
    (declare-function gr/erlang-rebar-code-path nil)
    (declare-function gr/erlang-rebar-include-path nil)
    (declare-function gr/erlang-rebar-hook nil))

  :init
  (defun gr/erlang-rebar-locate-root ()
    (locate-dominating-file default-directory "rebar.config"))


  (defun gr/erlang-rebar-code-path ()
    (split-string (shell-command-to-string "rebar3 path -s :") ":"))


  (defun gr/erlang-rebar-include-path ()
    (split-string (shell-command-to-string "find . -iname include -type d")))


  (defun gr/erlang-rebar-hook ()
    (let ((default-directory (gr/erlang-rebar-locate-root)))
      (when default-directory
        (progn
          (let ((paths (gr/erlang-rebar-code-path)))
            (progn
              (setq flycheck-erlang-library-path paths)
              (add-to-list 'inferior-erlang-machine-options "-pa" t)
              (dolist (path paths)
                (add-to-list 'inferior-erlang-machine-options path t))))
          (dolist (path (gr/erlang-rebar-include-path))
            (add-to-list 'flycheck-erlang-include-path path t))))))

  (with-eval-after-load 'flycheck
    (add-hook 'erlang-mode-hook 'gr/erlang-rebar-hook))

  :config
  (setq erlang-compile-extra-opts '(debug-info))
  (require 'erlang-start))
