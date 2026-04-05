{
  lib,
  config,
  pkgs,
  microvm,
  sops-nix,
  home-manager,
  dotfiles,
  ...
}:

{
  imports = [
    ../common.nix
    ../common-zfs.nix

    # profiles
    ../system/microvm.nix
    ../system/nvidia.nix
    ../system/pcie-aspm.nix
    ../system/vfio.nix

    # services
    ../services/fwupd.nix
    ../services/libvirtd.nix
    ../services/node-exporter-ipmitool.nix
    ../services/node-exporter-nvidia-gpu.nix
    ../services/node-exporter.nix
    ../services/podman.nix
    ../services/prometheus-agent.nix
    ../services/sanoid.nix
    ../services/syncoid.nix
  ];

  networking.hostName = "terra";
  networking.hostId = "ae419c0f";

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
  vfio.udev-forwarder.config.domain = [
    {
      name = "lunar";
      match = [
        { devpath = "/devices/pci0000:00/0000:00:14.0/usb1/1-4/1-4.1/1-4.1.4"; }
        { devpath = "/devices/pci0000:00/0000:00:14.0/usb1/1-4/1-4.2"; }
        { devpath = "/devices/pci0000:00/0000:00:14.0/usb1/1-4/1-4.3"; }
        { devpath = "/devices/pci0000:00/0000:00:14.0/usb1/1-4/1-4.4"; }
      ];
    }
  ];

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

  # Xeon w9-3400 does not allow clock frequency to spike too much when EPB is
  # set to something other than 1 ("performance"). This is not to be confused with
  # energy_performance_preference (EPP) which power-profiles-daemon uses to control
  # its performance mode.
  systemd.services = {
    "configure-intel-epb" = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        for n in /sys/devices/system/cpu/cpu*/power/energy_perf_bias; do
          if ! [ -f "$n" ]; then
            echo "Could not set Energy Performance Bias"
            exit 1
          fi
          echo 0 > $n
        done
      '';
    };
  };
}
