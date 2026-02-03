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
      set -ga terminal-overrides ",xterm*:Tc"
      set -wg automatic-rename on
      set -wg automatic-rename-format "#{pane_current_path} — #{pane_current_command}"

      set -g status-left "#[fg=colour6,bg=colour8,bold] #S@#h #[default,fg=colour8] "
      set -g status-left-style ""
      set -g status-left-length 32
      set -g status-right "#[fg=white,bg=black]#[fg=black,bg=white] %H:%M "
      set -g status-right-style ""
      set -g status-style bg=default
      set -wg window-status-current-format "#[fg=white,bold]‹#I› #{pane_current_command}"
      set -wg window-status-current-style ""
      set -wg window-status-format "#[fg=colour8]‹#I› #{pane_current_command}"
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
