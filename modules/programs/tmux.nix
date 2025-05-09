{ config, lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    escapeTime = 0;
    shell = "${config.programs.fish.package}/bin/fish";
    mouse = true;

    extraConfig = ''
      set -g default-command "exec ${config.programs.tmux.shell}"
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

      ${lib.optionalString pkgs.stdenv.isDarwin ''
        bind -T copy-mode M-w send -X copy-pipe-and-cancel "pbcopy"
        bind -T copy-mode-vi y send -X copy-pipe-and-cancel "pbcopy"
      ''}
    '';
  };
}
