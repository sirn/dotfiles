{ lib, pkgs, ... }:

let
  wakeupMonitor = pkgs.writeShellApplication {
    name = "wakeup-monitor";
    runtimeInputs = with pkgs; [
      coreutils
      findutils
      gawk
      gnugrep
      gnused
      systemd
    ];
    text = builtins.readFile ./wakeup-monitor.sh;
  };
in
{
  environment.systemPackages = [ wakeupMonitor ];

  systemd.tmpfiles.rules = [ "d /var/lib/wakeup-monitor 0755 root root -" ];

  systemd.services.wakeup-monitor-debug = {
    description = "Enable wakeup diagnostics";
    wantedBy = [ "multi-user.target" ];
    after = [ "local-fs.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${wakeupMonitor}/bin/wakeup-monitor debug";
      RemainAfterExit = true;
    };
  };

  powerManagement.powerDownCommands = lib.mkBefore ''
    ${wakeupMonitor}/bin/wakeup-monitor pre
  '';

  powerManagement.resumeCommands = lib.mkBefore ''
    ${wakeupMonitor}/bin/wakeup-monitor post
  '';
}
