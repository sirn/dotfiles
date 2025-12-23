{ config, lib, pkgs, ... }:

let
  cfg = config.services.swayidle;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  swaymsgBin = "${swaycfg.package}/bin/swaymsg";

  niriBin = "${niricfg.package}/bin/niri";

  displayControl = pkgs.writeScriptBin "display-control" ''
    #!${pkgs.runtimeShell}

    _run_sway() {
      command=$1
      if [ "$command" = "off" ]; then
        ${swaymsgBin} "output * dpms off"
      elif [ "$command" = "on" ]; then
        ${swaymsgBin} "output * dpms on"
      fi
    }

    _run_niri() {
      command=$1
      if [ "$command" = "off" ]; then
        ${niriBin} msg action power-off-monitors
      elif [ "$command" = "on" ]; then
        ${niriBin} msg action power-on-monitors
      fi
    }

    main() {
      if [ "$XDG_CURRENT_DESKTOP" = "sway" ]; then
        _run_sway "$@"
      elif [ "$XDG_CURRENT_DESKTOP" = "niri" ]; then
        _run_niri "$@"
      fi
    }

    main "$@"
  '';
in
{
  services.swayidle = {
    enable = true;

    timeouts = [
      {
        timeout = 180;
        command = "${lib.getExe displayControl} off";
        resumeCommand = "${lib.getExe displayControl} on";
      }
    ];
  };

  systemd.user.services.swayidle.Service = lib.mkIf cfg.enable {
    Slice = lib.mkDefault "app.slice";
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      keybindings = {
        "${swaycfg.config.modifier}+Ctrl+Shift+L" = "exec pkill -USR1 -f ${lib.getExe cfg.package}";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+Alt+L".action.spawn = [
          "pkill"
          "-USR1"
          "-f"
          "${lib.getExe cfg.package}"
        ];
      };
    };
  };
}
