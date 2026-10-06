{ config, lib, ... }:

{
  imports = [ ./base.nix ];

  networking = {
    useNetworkd = lib.mkForce false;
    useDHCP = lib.mkForce false;

    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
  };

  systemd.user.services.nm-file-secret-agent =
    lib.mkIf (config.networking.networkmanager.ensureProfiles.secrets.entries != [ ])
      {
        inherit (config.systemd.services.nm-file-secret-agent) description documentation script;
        wantedBy = [ "default.target" ];
      };

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (action.id == "org.freedesktop.NetworkManager.settings.modify.system" &&
          subject.isInGroup("wheel")) {
        return polkit.Result.YES;
      }
    });
  '';
}
