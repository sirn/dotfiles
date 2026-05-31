{
  nixos = {
    imports = [
      ../nixos/config/common.nix
      ../nixos/config/common-zfs.nix
      ../nixos/config/system/network/networkd.nix

      # profiles
      ../nixos/config/system/pcie-aspm.nix
      ../nixos/config/system/vfio.nix

      # services
      ../nixos/config/services/fwupd.nix
      ../nixos/config/services/libvirtd.nix
      ../nixos/config/services/node-exporter-ipmitool.nix
      ../nixos/config/services/node-exporter.nix
      ../nixos/config/services/podman.nix
      ../nixos/config/services/powerband.nix
      ../nixos/config/services/prometheus-agent.nix
      ../nixos/config/services/sanoid.nix
      ../nixos/config/services/syncoid.nix
    ];

    networking.hostName = "terra";

    systemd.network.netdevs = {
      "20-br0" = {
        netdevConfig = {
          Kind = "bridge";
          Name = "br0";
        };
      };
    };

    systemd.network.networks = {
      "30-eno2" = {
        matchConfig.Name = "eno2";
        networkConfig.Bridge = "br0";
        linkConfig.RequiredForOnline = "enslaved";
      };
      "35-microvm" = {
        matchConfig.Name = "vm-*";
        networkConfig.Bridge = "br0";
        linkConfig.RequiredForOnline = "no";
      };
      "40-br0" = {
        matchConfig.Name = "br0";
        networkConfig.IPv6AcceptRA = true;
        networkConfig.DHCP = "ipv4";
        linkConfig.RequiredForOnline = "carrier";
      };
    };

    # Terra is always connected, so it makes sense to wait for online
    systemd.network.wait-online.enable = true;

    vfio.iommuType = "intel_sm";

    services.sanoid.datasets."zroot/DATA" = {
      useTemplate = [ "daily" ];
      processChildrenOnly = true;
      recursive = true;
    };

    services.sanoid.datasets."zroot/ROOT" = {
      useTemplate = [ "daily" ];
      processChildrenOnly = true;
      recursive = true;
    };

  };

  home = {
    imports = [
      ../home-manager/config/common.nix
      ../home-manager/config/common-linux.nix
    ];
  };
}
