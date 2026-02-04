{ config, lib, pkgs, ... }:

let
  cfg = config.programs.tmux;
in
{
  programs.tmux = {
    enable = true;
    escapeTime = 0;
    shell = config.machine.interactiveShell;
    terminal = "screen-256color";
    mouse = true;

    extraConfig = ''
      set -ga update-environment " SSH_TTY"
      set -g default-command "exec ${cfg.shell}"
      set -g set-clipboard on
      set -g set-titles on
      set -g set-titles-string "#W via tmux: #S"
      set -ga terminal-features ",xterm*:RGB"
      set -ga terminal-features ",xterm*:clipboard"
      set -wg automatic-rename off

      set -g status-left ""

      set -g status-left-length 24
      set -g status-right "#[fg=colour6,bg=colour235]┃ #S@#h #[fg=colour250,bg=colour238] %H:%M "
      set -g status-right-style ""
      set -g status-style bg=default
      set -wg window-status-current-format "#[fg=colour6]┃#[fg=black,bg=colour6] #I #[fg=colour255,bg=colour240] #{window_name} "
      set -wg window-status-current-style ""
      set -g window-status-separator ""
      set -wg window-status-format " #[fg=colour245,bg=colour238] #I #[fg=colour250,bg=colour235] #{window_name} "
      set -wg window-status-style ""

      bind -T prefix r source-file "${config.home.homeDirectory}/.config/tmux/tmux.conf"
      bind -T copy-mode-vi v send -X begin-selection

      bind -T prefix X resize-pane -x 85%
      bind -T prefix Y resize-pane -y 85%

      ${lib.optionalString pkgs.stdenv.isDarwin ''
        bind -T copy-mode M-w send -X copy-pipe-and-cancel "pbcopy"
        bind -T copy-mode-vi y send -X copy-pipe-and-cancel "pbcopy"
      ''}
    '';
  };

  home.file = {
    ".tmux_init" = {
      executable = true;
      text = ''
        #!/bin/sh -l
        SESSION=$1

        if [ -z "$SESSION" ]; then
          SESSION=main
        fi

        # Update SSH_TTY for new panes
        export SSH_TTY=$(tty)

        ${if pkgs.stdenv.isLinux then ''
        exec systemd-run \
          --user \
          --scope \
          --slice=app.slice \
          --setenv=SSH_TTY="$SSH_TTY" \
          ${cfg.package}/bin/tmux new-session -A -s "$SESSION"
        '' else ''
        exec ${cfg.package}/bin/tmux new-session -A -s "$SESSION"
        ''}
      '';
    };
  };
}
