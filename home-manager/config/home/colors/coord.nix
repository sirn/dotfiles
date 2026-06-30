{
  theme,
  config,
  lib,
  stripHash,
  ...
}:

let
  s = theme.semantic;
in
lib.mkIf config.services.coord.enable {
  services.coord.style = {
    backgroundColor = "#${stripHash s.primary.bg}80";
    highlightColor = s.focus.bg;
    gridLineColor = "#${stripHash s.outline}59";
    fontColor = s.primary.text;
    cursorColor = s.tertiary.bg;
  };
}
