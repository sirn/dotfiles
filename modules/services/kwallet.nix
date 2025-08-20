{ config, lib, pkgs, ... }:

{
  systemd.user.services.kwallet-pam = lib.mkIf config.machine.isNixOS {
    Unit = {
      Description = "KWallet PAM Init";
      PartOf = [ config.wayland.systemd.target ];

      # Avoid re-running kwallet-pam.
      X-RestartIfChanged = false;
    };

    Service = {
      Type = "oneshot";
      ExecStart = with pkgs; "-${kdePackages.kwallet-pam}/libexec/pam_kwallet_init";
    };

    Install = {
      WantedBy = [ config.wayland.systemd.target ];
    };
  };
}
