{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.tmux;
in
{
  programs.tmux = {
    enable = true;
    escapeTime = 0;
    shell = config.home.shell.interactiveShell;
    terminal = "tmux-256color";
    mouse = true;

    extraConfig = ''
      set -ga update-environment " SSH_TTY"
      set -g default-command "exec ${cfg.shell}"
      set -g set-clipboard on
      set -g set-titles on
      set -g extended-keys on
      set -g extended-keys-format csi-u
      set -g set-titles-string "#W via tmux: #S"
      # xterm-compatible baseline (Alacritty, WezTerm, Foot, Ghostty, etc.)
      set -ga terminal-features ",xterm*:RGB"
      set -ga terminal-features ",xterm*:clipboard"
      set -ga terminal-features ",xterm*:cstyle"
      set -ga terminal-features ",xterm*:ccolour"
      set -ga terminal-features ",xterm*:hyperlinks"
      set -ga terminal-features ",xterm*:strikethrough"
      set -ga terminal-features ",xterm*:title"
      set -ga terminal-features ",xterm*:focus"

      # Wezterm-specific (additional features)
      set -ga terminal-features ",wezterm:RGB"
      set -ga terminal-features ",wezterm:usstyle"
      set -ga terminal-features ",wezterm:cstyle"
      set -ga terminal-features ",wezterm:clipboard"
      set -ga terminal-features ",wezterm:sync"
      set -ga terminal-features ",wezterm:strikethrough"
      set -ga terminal-features ",wezterm:overline"
      set -ga terminal-features ",wezterm:hyperlinks"
      set -ga terminal-features ",wezterm:focus"
      set -ga terminal-features ",wezterm:title"
      set -ga terminal-features ",wezterm:ccolour"
      set -g allow-passthrough on
      set -wg automatic-rename off

      set -g status-left ""

      set -g status-left-length 24
      set -g status-right "#[fg=#{@color_inactive_text},bg=#{@color_inactive_bg}]┃ #S @ #h #[fg=#{@color_primary_text},bg=#{@color_selection_bg}] %H:%M "
      set -g status-right-style ""
      set -g status-style bg=default
      set -wg window-status-current-format "#[fg=#{@color_focus_bg}]┃#[fg=#{@color_focus_text},bg=#{@color_focus_bg}] #I #[fg=#{@color_primary_text},bg=#{@color_selection_bg}] #{window_name} "
      set -wg window-status-current-style ""
      set -g window-status-separator ""
      set -wg window-status-format " #[fg=#{@color_inactive_text},bg=#{@color_inactive_bg}] #I #[fg=#{@color_primary_text},bg=#{@color_primary_bg}] #{window_name} "
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

        # If already in a tmux session, create detached and switch.
        # Note: switch-client just tells the server to switch and exits immediately,
        # so we must not exec here or the pane will close. The caller decides whether
        # to exec this script based on $TMUX.
        if [ -n "$TMUX" ]; then
          ${cfg.package}/bin/tmux new-session -d -s "$SESSION" 2>/dev/null || true
          ${cfg.package}/bin/tmux switch-client -t "$SESSION"
          return
        fi

        ${
          if pkgs.stdenv.isLinux then
            ''
              exec systemd-run \
                --user \
                --scope \
                --slice=app.slice \
                --setenv=SSH_TTY="$SSH_TTY" \
                ${cfg.package}/bin/tmux new-session -A -s "$SESSION"
            ''
          else
            ''
              exec ${cfg.package}/bin/tmux new-session -A -s "$SESSION"
            ''
        }
      '';
    };
  };
}
