{
  theme,
  config,
  lib,
  stripHash,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.programs.fuzzel.enable {
  programs.fuzzel.settings.colors = {
    background = "${semantic.primary.bg}fa";
    selection = "${semantic.focus.bg}ff";
    border = "${semantic.focus.bg}99";
    text = "${semantic.primary.text}ff";
    match = "${semantic.primary.text}ff";
    selection-text = "${semantic.focus.text}ff";
    selection-match = "${semantic.focus.text}ff";
  };
}
