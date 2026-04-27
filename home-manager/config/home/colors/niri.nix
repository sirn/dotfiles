{
  theme,
  config,
  lib,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.programs.niri.enable {
  programs.niri.settings.layout.focus-ring = {
    active.color = "${semantic.focus.bg}99";
    inactive.color = "${semantic.inactive.bg}99";
    urgent.color = semantic.urgent.bg;
  };
}
