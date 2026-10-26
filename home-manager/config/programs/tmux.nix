{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.tmux = {
    enable = true;
    escapeTime = 0;
    terminal = "tmux-256color";
    mouse = true;
    historyLimit = 50000;
    baseIndex = 1;
    focusEvents = true;
    aggressiveResize = false;
    keyMode = "vi";

    extraConfig = ''
      set -g @color_primary_bg "default"
      set -g @color_primary_text "default"
      set -g @color_accent_bg "blue"
      set -g @color_accent_text "brightwhite"
      set -g @color_muted_bg "brightblack"
      set -g @color_muted_text "brightwhite"
      set -g @color_focus_bg "blue"
      set -g @color_focus_text "brightwhite"

      set -ga update-environment " SSH_TTY"
      set -g set-clipboard on
      set -g set-titles on
      set -g extended-keys on
      set -g extended-keys-format csi-u
      set -g set-titles-string "#W via tmux: #S"
      # xterm-compatible baseline (Alacritty, Foot, Ghostty via xterm-ghostty, etc.)
      # Ghostty sets TERM=xterm-ghostty (or xterm-256color), so xterm* covers it.
      set -ga terminal-features ",xterm*:ccolour"
      set -ga terminal-features ",xterm*:clipboard"
      set -ga terminal-features ",xterm*:cstyle"
      set -ga terminal-features ",xterm*:extkeys"
      set -ga terminal-features ",xterm*:margins"
      set -ga terminal-features ",xterm*:osc7"
      set -ga terminal-features ",xterm*:focus"
      set -ga terminal-features ",xterm*:hyperlinks"
      set -ga terminal-features ",xterm*:overline"
      set -ga terminal-features ",xterm*:RGB"
      set -ga terminal-features ",xterm*:strikethrough"
      set -ga terminal-features ",xterm*:sync"
      set -ga terminal-features ",xterm*:title"
      set -ga terminal-features ",xterm*:usstyle"

      # WezTerm sets TERM=wezterm (no xterm prefix), so it needs its own block.
      set -ga terminal-features ",wezterm:ccolour"
      set -ga terminal-features ",wezterm:clipboard"
      set -ga terminal-features ",wezterm:cstyle"
      set -ga terminal-features ",wezterm:extkeys"
      set -ga terminal-features ",wezterm:margins"
      set -ga terminal-features ",wezterm:osc7"
      set -ga terminal-features ",wezterm:focus"
      set -ga terminal-features ",wezterm:hyperlinks"
      set -ga terminal-features ",wezterm:overline"
      set -ga terminal-features ",wezterm:RGB"
      set -ga terminal-features ",wezterm:strikethrough"
      set -ga terminal-features ",wezterm:sync"
      set -ga terminal-features ",wezterm:title"
      set -ga terminal-features ",wezterm:usstyle"

      # With mouse on, OSC 8 hyperlinks need Shift+click (or Cmd+Shift+click on macOS)
      # to bypass tmux's mouse capture and let the terminal handle the click.
      set -g allow-passthrough on
      set -wg automatic-rename off

      set -g pane-base-index 1
      set -g renumber-windows on
      set -g display-time 4000
      set -g status-interval 5

      set -g status-left ""

      set -g status-left-length 24
      set -g status-right "#[fg=#{@color_muted_text},bg=#{@color_muted_bg}]┃ #S @ #h #[fg=#{@color_accent_text},bg=#{@color_accent_bg}] %H:%M "
      set -g status-right-style ""
      set -g status-style bg=default
      set -wg window-status-current-format "#[fg=#{@color_focus_bg}]┃#[fg=#{@color_focus_text},bg=#{@color_focus_bg}] #I #[fg=#{@color_muted_text},bg=#{@color_muted_bg}] #{window_name} "
      set -wg window-status-current-style ""
      set -g window-status-separator ""
      set -wg window-status-format " #[fg=#{@color_muted_text},bg=#{@color_muted_bg}] #I #[fg=#{@color_primary_text},bg=#{@color_primary_bg}] #{window_name} "
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
}
