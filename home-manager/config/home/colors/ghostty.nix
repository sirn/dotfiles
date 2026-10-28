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
    cursor-color = stripHash t.base16Colors.cursor;
    foreground = stripHash t.base16Colors.foreground;
    # Explicit extended colors would prevent native palette generation.
    palette = lib.imap0 (i: c: "${toString i}=${stripHash c}") (lib.take 16 t.palette256);
    selection-background = stripHash t.base16Colors.selection;
    selection-foreground = stripHash t.base16Colors.selectionFg;
  };
in
lib.mkIf config.programs.ghostty.enable {
  programs.ghostty = {
    settings = {
      theme = ghosttyThemeName;
      palette-generate = true;
      # Follow background → foreground instead of fixed dark → light ordering.
      palette-harmonious = true;
    };
    themes = lib.mapAttrs (name: ghosttyTheme) themes;
  };
}
