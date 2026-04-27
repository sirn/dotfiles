{
  theme,
  config,
  lib,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.programs.omniwm.enable { programs.omniwm.borders.color = "${semantic.focus.bg}99"; }
