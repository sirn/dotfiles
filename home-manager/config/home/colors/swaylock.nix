{
  theme,
  config,
  lib,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.programs.swaylock.enable { programs.swaylock.settings.color = semantic.primary.bg; }
