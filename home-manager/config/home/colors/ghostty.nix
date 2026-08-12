{
  themes,
  schemes,
  familyName,
  config,
  lib,
  stripHash,
  ...
}:

let
  family = schemes.${familyName};
  lightThemeName = family.light.name;
  darkThemeName = family.dark.name;

  ghosttyThemeName =
    if config.home.colors.variants.terminal == "dark" then
      darkThemeName
    else if config.home.colors.variants.terminal == "light" then
      lightThemeName
    else
      "light:${lightThemeName},dark:${darkThemeName}";

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
    settings.theme = ghosttyThemeName;
    themes = lib.mapAttrs (name: ghosttyTheme) themes;
  };
}
