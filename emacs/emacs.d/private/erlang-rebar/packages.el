(setq erlang-rebar-packages
      '(erlang
        (erlang-rebar :location local)))

(defun erlang-rebar/init-erlang-rebar ()
  (use-package erlang-rebar
    :defer t
    :init
    (autoload 'erlang-rebar-hook "erlang-rebar")))

(defun erlang-rebar/post-init-erlang ()
  (add-hook 'erlang-mode-hook 'erlang-rebar-hook))
