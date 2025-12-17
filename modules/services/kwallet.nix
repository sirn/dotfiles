{ config, lib, pkgs, ... }:

{
  systemd.user.services.kwallet-pam = lib.mkIf (pkgs.stdenv.isLinux && !config.targets.genericLinux.enable) {
    Unit = {
      Description = "KWallet PAM Init";
      PartOf = [ config.wayland.systemd.target ];

      # Avoid re-running kwallet-pam.
      X-RestartIfChanged = false;
    };

    Service = {
      Type = "oneshot";
      ExecStart = with pkgs; "-${kdePackages.kwallet-pam}/libexec/pam_kwallet_init";
      Slice = lib.mkDefault "app.slice";
      RemainAfterExit = "yes";
      KillMode = "control-group";
    };

    Install = {
      WantedBy = [ config.wayland.systemd.target ];
    };
  };
}
