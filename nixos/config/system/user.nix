{ config, pkgs, ... }:

{
  users.users = {
    sirn = {
      uid = 1000;
      isNormalUser = true;
      extraGroups = [
        "wheel"
      ]
      ++ (
        if config.virtualisation.libvirtd.enable then
          [
            "libvirtd"
            "kvm"
          ]
        else
          [ ]
      )
      ++ (if config.virtualisation.podman.enable then [ "podman" ] else [ ])
      ++ (if config.virtualisation.docker.enable then [ "docker" ] else [ ])
      ++ (
        if config.hardware.uinput.enable then
          [
            "uinput"
            "input"
          ]
        else
          [ ]
      );

      subUidRanges = [
        {
          startUid = 100000;
          count = 65536;
        }
      ];
      subGidRanges = [
        {
          startGid = 100000;
          count = 65536;
        }
      ];

      openssh.authorizedKeys.keyFiles = [
        (pkgs.fetchurl {
          url = "https://files.grid.in.th/ssh.keys";
          sha256 = "sha256-OKuBX1DbFv0E3620UASgHPKVYRsKtSVhSCxauyWYRyU=";
        })
      ];
    };
  };

  users.groups = {
    sirn = {
      gid = 1000;
      members = [ "sirn" ];
    };
  };

  # Make /etc/nixos writable by users group by default.
  systemd.tmpfiles.settings = {
    "nixos-user" = {
      "/etc/nixos" = {
        d = {
          group = "wheel";
          mode = "0775";
          user = "root";
        };
      };
    };
  };
}
