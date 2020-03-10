(use-package erlang
  :interpreter "erl"
  :mode ("\\.erl\\'" . erlang-mode)
  :straight t

  :preface
  (eval-when-compile
    (defvar erlang-compile-function)
    (defvar inferior-erlang-machine)
    (defvar inferior-erlang-machine-options)
    (defvar inferior-erlang-shell-type)
    (declare-function inferior-erlang-prepare-for-input nil)
    (declare-function inferior-erlang-send-command nil)
    (declare-function inferior-erlang-wait-prompt nil)
    (declare-function gr/erlang-rebar3-locate-root nil)
    (declare-function gr/erlang-rebar3-compile nil)
    (declare-function gr/erlang-rebar3-hook nil)
    (declare-function gr/erlang-rebar3-wrap-maybe nil))

  :init
  (defun gr/erlang-rebar3-locate-root ()
    (locate-dominating-file default-directory "rebar.config"))

  ;; Copy of inferior-erlang-compile with r3:compile(). instead of c(path).
  (defun gr/erlang-rebar3-compile (_arg)
    (interactive "P")
    (save-some-buffers)
    (inferior-erlang-prepare-for-input)
    (with-current-buffer inferior-erlang-buffer
      (when (fboundp 'compilation-forget-errors)
        (compilation-forget-errors)))
    (let (end)
      (setq end (inferior-erlang-send-command "r3:compile()." nil))
      (sit-for 0)
      (inferior-erlang-wait-prompt)
      (with-current-buffer inferior-erlang-buffer
        (setq compilation-error-list nil)
        (set-marker compilation-parsing-end end)))
    (setq compilation-last-buffer inferior-erlang-buffer))

  (defun gr/erlang-rebar3-hook ()
    (when-let ((default-directory (gr/erlang-rebar3-locate-root)))
      (setq-local inferior-erlang-machine "rebar3")
      (setq-local inferior-erlang-machine-options '("shell"))
      (setq-local inferior-erlang-shell-type nil)
      (setq-local erlang-compile-function 'gr/erlang-rebar3-compile)))

  (defun gr/erlang-rebar3-wrap-maybe (orig-fun &rest args)
    (if-let ((default-directory (gr/erlang-rebar3-locate-root)))
        (apply orig-fun args)
      (apply orig-fun args)))

  (add-hook 'erlang-mode-hook 'flycheck-mode)
  (add-hook 'erlang-mode-hook 'gr/erlang-rebar3-hook)
  (advice-add 'inferior-erlang :around 'gr/erlang-rebar3-wrap-maybe)

  :config
  (require 'erlang-start))
