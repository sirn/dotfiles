{
  nixos = {
    imports = [
      ../nixos/config/common.nix
      ../nixos/config/system/network/networkd.nix
      ../nixos/config/services/podman.nix
    ];

    hardware.enableRedistributableFirmware = false;

    users.users.sirn.linger = true;

    systemd.network.networks = {
      "20-veth-unmanaged" = {
        matchConfig.Name = "veth*";
        linkConfig.Unmanaged = true;
      };
      "90-ignore-tun" = {
        matchConfig.Name = "tun*";
        linkConfig.Unmanaged = true;
      };
      "99-dhcp" = {
        matchConfig.Name = "*";
        networkConfig = {
          IPv6AcceptRA = true;
          DHCP = "ipv4";
        };
      };
    };
  };

  home = {
    imports = [
      ../home-manager/config/common.nix
      ../home-manager/config/common-linux.nix
    ];
  };
}
