{
  imports = [
    ../common.nix
    ../common-zfs.nix

    # profiles
    ../system/bluetooth.nix
    ../system/desktop.nix
    ../system/game.nix
    ../system/intel-gpu.nix
    ../system/plasma.nix
    ../system/wireless.nix

    # programs
    ../programs/1password.nix

    # services
    ../services/flatpak.nix
    ../services/node-exporter-ipmitool.nix
    ../services/node-exporter.nix
    ../services/podman.nix
    ../services/prometheus-agent.nix
    ../services/sddm.nix
  ];

  networking.hostId = "4d91fe14";
  networking.hostName = "phoebe";
}
