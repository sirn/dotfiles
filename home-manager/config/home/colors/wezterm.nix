{
  themes,
  config,
  lib,
  ...
}:

let
  weztermColorScheme = t: {
    ansi = [
      t.base16Colors.normal.black
      t.base16Colors.normal.red
      t.base16Colors.normal.green
      t.base16Colors.normal.yellow
      t.base16Colors.normal.blue
      t.base16Colors.normal.magenta
      t.base16Colors.normal.cyan
      t.base16Colors.normal.white
    ];
    brights = [
      t.base16Colors.bright.black
      t.base16Colors.bright.red
      t.base16Colors.bright.green
      t.base16Colors.bright.yellow
      t.base16Colors.bright.blue
      t.base16Colors.bright.magenta
      t.base16Colors.bright.cyan
      t.base16Colors.bright.white
    ];
    indexed = lib.listToAttrs (
      lib.imap0 (i: c: {
        name = toString i;
        value = c;
      }) t.palette256
    );
    background = t.base16Colors.background;
    cursor_bg = t.base16Colors.foreground;
    cursor_border = t.base16Colors.foreground;
    cursor_fg = t.base16Colors.background;
    foreground = t.base16Colors.foreground;
    scrollbar_thumb = t.base16Colors.scrollbar;
    selection_bg = t.base16Colors.selection;
    selection_fg = t.base16Colors.background;
  };
in
lib.mkIf config.programs.wezterm.enable {
  programs.wezterm.colorSchemes = lib.mapAttrs (name: weztermColorScheme) themes;
}
