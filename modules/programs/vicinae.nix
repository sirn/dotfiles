{ config, lib, ... }:

let
  cfg = config.programs.vicinae;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;
in
{
  programs.vicinae = {
    enable = true;

    systemd = {
      enable = true;
    };
  };

  systemd.user.services.vicinae.Service = lib.mkIf (cfg.enable && cfg.systemd.enable) {
    Slice = lib.mkDefault "app.slice";
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      keybindings = {
        "${swaycfg.config.modifier}+d" = "exec ${lib.getExe cfg.package} open";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+d".action.spawn = [ "${lib.getExe cfg.package}" "open" ];
      };
    };
  };
}
