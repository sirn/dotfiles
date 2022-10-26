{ config, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
  inherit (config.home) homeDirectory;
  inherit (config.lib.file) mkOutOfStoreSymlink;

  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
{
  programs.tmux = {
    enable = true;
    escapeTime = 0;
    shell = "$SHELL";
    terminal = "screen-256color";
    tmuxp.enable = true;

    extraConfig = ''
      set -g mouse on
      set -g set-clipboard on
      set -g set-titles on
      set -g set-titles-string "#W via tmux: #S"
      set -ga terminal-overrides ",xterm*:Tc"
      set -wg automatic-rename on
      set -wg automatic-rename-format "#{pane_current_path} — #{pane_current_command}"

      set -g status-left "#[fg=white,bg=colour30,bold] #S@#h #[fg=colour30,bg=default] "
      set -g status-left-style ""
      set -g status-left-length 32
      set -g status-right "#[fg=white,bg=black]#[fg=black,bg=white] %H:%M "
      set -g status-right-style ""
      set -g status-style bg=default
      set -wg window-status-current-format "#[fg=white,bold]‹#I› #{pane_current_command}"
      set -wg window-status-current-style ""
      set -wg window-status-format "#[fg=colour59]‹#I› #{pane_current_command}"
      set -wg window-status-style ""

      bind -T prefix r source-file "${homeDirectory}/.config/tmux/tmux.conf"
      bind -T copy-mode-vi v send -X begin-selection
    '' + (if !isDarwin then "" else ''
      bind -T copy-mode M-w send -X copy-pipe-and-cancel "pbcopy"
      bind -T copy-mode-vi y send -X copy-pipe-and-cancel "pbcopy"
    '');
  };

  home.file = {
    ".tmuxp" = { source = mkOutOfStoreSymlink "${dotprivDir}/etc/tmuxp"; };
  };
}
