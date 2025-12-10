{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  swayidlecfg = config.services.swayidle;

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
    ]
    ++ (if config.machine.isLaptop then [
      {
        timeout = 300;
        command = "${config.systemd.user.systemctlPath} suspend";
      }
    ] else [ ]);
  };

  systemd.user.services.swayidle.Service = {
    Slice = lib.mkDefault "session.slice";
  };

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      keybindings = {
        "${swaycfg.config.modifier}+Ctrl+Shift+L" = "exec pkill -USR1 -f ${lib.getExe swayidlecfg.package}";
      };
    };
  };

  programs.niri = lib.mkIf niricfg.enable {
    settings = {
      binds = {
        "Mod+Alt+L".action.spawn = [
          "pkill"
          "-USR1"
          "-f"
          "${lib.getExe swayidlecfg.package}"
        ];
      };
    };
  };
}
