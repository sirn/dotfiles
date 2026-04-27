{
  themes,
  themeName,
  config,
  lib,
  stripHash,
  ...
}:

let
  ghosttyTheme = t: {
    background = stripHash t.base16Colors.background;
    cursor-color = stripHash t.base16Colors.foreground;
    foreground = stripHash t.base16Colors.foreground;
    palette = lib.imap0 (i: c: "${toString i}=${stripHash c}") t.palette256;
    selection-background = stripHash t.base16Colors.selection;
    selection-foreground = stripHash t.base16Colors.background;
  };
in
lib.mkIf config.programs.ghostty.enable {
  programs.ghostty = {
    settings.theme = themeName;
    themes = lib.mapAttrs (name: ghosttyTheme) themes;
  };
}
