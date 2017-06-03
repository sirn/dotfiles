(req-package exec-path-from-shell)

(req-package keychain-environment
  :require exec-path-from-shell
  :config
  (let ((shell (getenv "SHELL")))
    (progn
      (setenv "SHELL" "/bin/sh")
      (keychain-refresh-environment)
      (setenv "SHELL" shell))))
