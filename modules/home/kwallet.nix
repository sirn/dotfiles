{ config, pkgs, ... }:

{
  systemd.user.services.kwallet-pam = {
    Unit = {
      Description = "KWallet PAM2 Init";
      PartOf = [ config.wayland.systemd.target ];
    };

    Service = {
      Type = "oneshot";
      ExecStart = with pkgs; "${kdePackages.kwallet-pam}/libexec/pam_kwallet_init";
    };

    Install = {
      WantedBy = [ config.wayland.systemd.target ];
    };
  };
}
