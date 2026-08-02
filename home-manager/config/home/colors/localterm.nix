{
  schemes,
  familyName,
  config,
  lib,
  ...
}:

let
  toLocalTermTheme = variant: label: {
    id = "nix-${familyName}-${label}";
    name = "${familyName}-${label}";
    source = "nix-dotfiles";
    colors = {
      background = variant.base16Colors.background;
      foreground = variant.base16Colors.foreground;
      cursor = variant.base16Colors.foreground;
      cursorAccent = variant.base16Colors.background;
      selectionBackground = variant.base16Colors.selection;
      selectionForeground = variant.base16Colors.background;
      black = variant.base16Colors.normal.black;
      red = variant.base16Colors.normal.red;
      green = variant.base16Colors.normal.green;
      yellow = variant.base16Colors.normal.yellow;
      blue = variant.base16Colors.normal.blue;
      magenta = variant.base16Colors.normal.magenta;
      cyan = variant.base16Colors.normal.cyan;
      white = variant.base16Colors.normal.white;
      brightBlack = variant.base16Colors.bright.black;
      brightRed = variant.base16Colors.bright.red;
      brightGreen = variant.base16Colors.bright.green;
      brightYellow = variant.base16Colors.bright.yellow;
      brightBlue = variant.base16Colors.bright.blue;
      brightMagenta = variant.base16Colors.bright.magenta;
      brightCyan = variant.base16Colors.bright.cyan;
      brightWhite = variant.base16Colors.bright.white;
    };
  };

  family = schemes.${familyName};
in
lib.mkIf config.services.localterm.enable {
  home.file.".localterm/themes.json" = {
    text = builtins.toJSON {
      version = 1;
      activeThemeId = "nix-${familyName}-${config.home.colors.variants.terminal}";
      lightThemeId = "nix-${familyName}-light";
      darkThemeId = "nix-${familyName}-dark";
      customThemes = [
        (toLocalTermTheme family.light "light")
        (toLocalTermTheme family.dark "dark")
      ];
    };
  };
}