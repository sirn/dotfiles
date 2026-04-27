{
  theme,
  config,
  lib,
  ...
}:

lib.mkIf config.programs.alacritty.enable {
  programs.alacritty.settings.colors = {
    primary = {
      background = theme.base16Colors.background;
      foreground = theme.base16Colors.foreground;
    };
    normal = theme.base16Colors.normal;
    bright = theme.base16Colors.bright;
  };
}
