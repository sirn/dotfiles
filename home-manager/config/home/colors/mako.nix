{
  theme,
  config,
  lib,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.services.mako.enable {
  services.mako.settings = {
    background-color = "${semantic.primary.bg}fa";
    text-color = semantic.primary.text;
    border-color = "${semantic.focus.bg}99";
    progress-color = "${semantic.focus.bg}55";

    "urgency=low" = {
      border-color = semantic.inactive.bg;
    };

    "urgency=critical" = {
      border-color = semantic.urgent.bg;
      default-timeout = 0;
    };
  };
}
