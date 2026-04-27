{
  theme,
  config,
  lib,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.wayland.windowManager.sway.enable {
  wayland.windowManager.sway.config.colors = {
    focused = {
      background = "${semantic.focus.bg}99";
      border = "${semantic.focus.bg}99";
      childBorder = "${semantic.focus.bg}99";
      indicator = "${semantic.focus.bg}a5";
      text = semantic.focus.text;
    };
    focusedInactive = {
      background = "${semantic.inactive.bg}99";
      border = "${semantic.inactive.bg}a5";
      childBorder = "${semantic.inactive.bg}66";
      indicator = "${semantic.inactive.bg}50";
      text = semantic.inactive.text;
    };
    unfocused = {
      background = "${semantic.primary.bg}99";
      border = "${semantic.primary.bg}a5";
      childBorder = "${semantic.primary.bg}66";
      indicator = "${semantic.primary.bg}ff";
      text = semantic.primary.text;
    };
    placeholder = {
      background = semantic.warning.bg;
      border = semantic.warning.bg;
      childBorder = semantic.warning.bg;
      indicator = semantic.warning.bg;
      text = semantic.warning.text;
    };
    urgent = {
      background = semantic.urgent.bg;
      border = semantic.urgent.bg;
      childBorder = semantic.urgent.bg;
      indicator = semantic.urgent.bg;
      text = semantic.urgent.text;
    };
  };
}
