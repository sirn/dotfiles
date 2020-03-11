;; -*- lexical-binding: t -*-

(use-package erlang
  :preface
  (eval-when-compile
    (defvar erlang-compile-function)
    (defvar inferior-erlang-machine)
    (defvar inferior-erlang-machine-options)
    (defvar inferior-erlang-shell-type)
    (declare-function inferior-erlang-prepare-for-input nil)
    (declare-function inferior-erlang-send-command nil)
    (declare-function inferior-erlang-wait-prompt nil)
    (declare-function gemacs--erlang-rebar3-locate-root nil)
    (declare-function gemacs--erlang-rebar3-compile nil)
    (declare-function gemacs--erlang-rebar3-hook nil)
    (declare-function gemacs--erlang-rebar3-wrap-maybe nil))

  :init
  (defun gemacs--erlang-rebar3-locate-root ()
    (locate-dominating-file default-directory "rebar.config"))

  ;; Copy of inferior-erlang-compile with r3:compile(). instead of c(path).
  (defun gemacs--erlang-rebar3-compile (_arg)
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

  (defun gemacs--erlang-rebar3-hook ()
    (when-let ((default-directory (gemacs--erlang-rebar3-locate-root)))
      (setq-local inferior-erlang-machine "rebar3")
      (setq-local inferior-erlang-machine-options '("shell"))
      (setq-local inferior-erlang-shell-type nil)
      (setq-local erlang-compile-function 'gemacs--erlang-rebar3-compile)))

  (defun gemacs--erlang-rebar3-wrap-maybe (orig-fun &rest args)
    (if-let ((default-directory (gemacs--erlang-rebar3-locate-root)))
        (apply orig-fun args)
      (apply orig-fun args)))

  (add-hook 'erlang-mode-hook #'gemacs--erlang-rebar3-hook)
  (advice-add 'inferior-erlang :around #'gemacs--erlang-rebar3-wrap-maybe)

  :config
  (require 'erlang-start))
