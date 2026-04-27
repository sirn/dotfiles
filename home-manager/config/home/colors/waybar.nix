{
  theme,
  config,
  lib,
  ...
}:

let
  semantic = theme.semantic;
in
lib.mkIf config.programs.waybar.enable {
  programs.waybar.style = lib.mkDefault ''
    @define-color default_bg ${semantic.primary.bg};
    @define-color default_text ${semantic.primary.text};
    @define-color highlight_bg ${semantic.focus.bg};
    @define-color highlight_text ${semantic.focus.text};
    @define-color alert_bg ${semantic.urgent.bg};
    @define-color alert_text ${semantic.primary.text};
    @define-color battery_charging_bg ${semantic.battery.charging.bg};
    @define-color battery_charging_text ${semantic.battery.charging.text};
    @define-color battery_warning_bg ${semantic.battery.low.bg};
    @define-color battery_warning_text ${semantic.battery.low.text};
    @define-color battery_critical_bg ${semantic.battery.critical.bg};
    @define-color battery_critical_text ${semantic.battery.critical.text};
    @define-color inactive_bg ${semantic.inactive.bg};
    @define-color inactive_text ${semantic.inactive.text};

    ${builtins.readFile ./waybar.css}
  '';
}
