(setq keychain-packages
      '(keychain-environment))

(defun keychain/init-keychain-environment ()
  (use-package keychain-environment
    :config
    (let ((shell (getenv "SHELL")))
      (progn
        (setenv "SHELL" "/bin/sh")
        (keychain-refresh-environment)
        (setenv "SHELL" shell)))))
