;; -*- lexical-binding: t -*-

(use-package rust-mode)


(use-package racer
  :init
  (add-hook 'rust-mode-hook #'racer-mode))
