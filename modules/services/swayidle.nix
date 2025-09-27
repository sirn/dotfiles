{ config, lib, pkgs, ... }:

let
  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  swayidlecfg = config.services.swayidle;

  swaymsgBin = "${swaycfg.package}/bin/swaymsg";

  niriBin = "${niricfg.package}/bin/niri";

  # power-on-monitors sometimes turn on the display when the machine is
  # waking up from s2idle while lid is closed for some reason; let's be
  # extra sure to not turn on eDP-1 if lid is closed
  displayControlNiriMaybeLaptopOn =
    if config.machine.isLaptop
    then
      ''
        LID_STATE=na

        for f in /proc/acpi/button/lid/*/state; do
          if [ -f "$f" ]; then
            LID_STATE=$(${lib.getExe pkgs.gawk} '{ print $2 }' <"$f")
            break
          fi
        done

        ${niriBin} msg -j outputs | ${lib.getExe pkgs.jq} -r "keys[]" | while read -r n; do
          if [ "$n" != "eDP-1" ] || [ "$LID_STATE" != "closed" ]; then
            ${niriBin} msg output "$n" on
          fi
        done
      ''
    else
      ''
        ${niriBin} msg action power-on-monitors
      '';

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
        ${displayControlNiriMaybeLaptopOn}
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
