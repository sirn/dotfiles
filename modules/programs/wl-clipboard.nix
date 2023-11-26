{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkOption types;

  cfg = config.services.wl-clipboard;
in
{
  options.services.wl-clipboard = {
    enable = mkEnableOption "wl-clipboard";

    package = mkOption
      {
        type = types.package;
        default = pkgs.wl-clipboard;
        defaultText = "pkgs.wl-clipboard";
        description = ''
          wl-clipboard derivation to use.
        '';
      };
  };

  config =
    mkIf config.desktop.enable {
      services.wl-clipboard = {
        enable = config.machine.isNixOS;
      };

      systemd.user.services = mkIf cfg.enable {
        wl-clipboard = {
          Unit = {
            Description = "Clipboard synchronization for Wayland compositors.";
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = "${cfg.package}/bin/wl-paste -pw ${cfg.package}/wl-copy";
          };

          Install = {
            WantedBy = [
              "graphical-session.target"
            ];
          };
        };
      };

      # non-NixOS; assume no systemd
      wayland.windowManager.sway =
        mkIf (!config.services.wl-clipboard.enable) {
          config = {
            startup = [
              { command = "${cfg.package}/bin/wl-paste -pw ${cfg.package}/wl-copy"; }
            ];
          };
        };
    };
}
