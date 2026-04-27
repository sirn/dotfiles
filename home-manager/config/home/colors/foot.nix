{
  theme,
  config,
  lib,
  stripHash,
  ...
}:

let
  palette256 = theme.palette256;
in
lib.mkIf config.programs.foot.enable {
  programs.foot.settings.colors = {
    background = stripHash theme.base16Colors.background;
    foreground = stripHash theme.base16Colors.foreground;
  }
  // lib.listToAttrs (
    lib.imap0 (i: c: {
      name = toString i;
      value = stripHash c;
    }) palette256
  );
}
