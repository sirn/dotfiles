{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.rustdesk;
in
{
  options.services.rustdesk = {
    enable = lib.mkEnableOption "the RustDesk remote desktop service";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.local.rustdesk-flutter-nightly;
      defaultText = lib.literalExpression "pkgs.local.rustdesk-flutter-nightly";
      description = "RustDesk package to run as the root service.";
    };
  };

  config = lib.mkIf cfg.enable {
    # The root service serves unattended connections and drives DRM capture.
    # Remote input needs uinput; DRM capture reads the host DRM devices, which
    # root can already access.
    hardware.uinput.enable = lib.mkDefault true;

    environment.systemPackages = [ cfg.package ];

    systemd.services.rustdesk = {
      description = "RustDesk remote desktop service";
      after = [
        "network.target"
        "systemd-user-sessions.service"
      ];
      wants = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        PULSE_LATENCY_MSEC = "60";
        PIPEWIRE_LATENCY = "1024/48000";
      };

      serviceConfig = {
        Type = "simple";
        ExecStart = "${lib.getExe cfg.package} --service";
        # The service spawns the session server and tray as children; mixed
        # mode reaps them when the main process exits.
        KillMode = "mixed";
        TimeoutStopSec = 30;
        LimitNOFILE = 100000;
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}
