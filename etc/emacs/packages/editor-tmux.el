(use-package emamux
  :straight t

  :init
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "ts" 'emamux:send-command
      "ty" 'emamux:yank-from-list-buffers
      "t!" 'emamux:run-command
      "tr" 'emamux:run-last-command
      "ts" 'emamux:run-region
      "tk" 'emamux:close-runner-pane
      "tK" 'emamux:close-panes
      "tc" 'emamux:new-window
      "tC" 'emamux:clone-current-frame
      "t-" 'emamux:split-window
      "t/" 'emamux:split-window-horizontally)))
