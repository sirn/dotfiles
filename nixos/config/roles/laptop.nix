{
  imports = [
    ../common.nix
    ../system/network/networkmanager.nix

    # System
    ../system/bluetooth.nix
    ../system/desktop.nix
    ../system/game.nix
    ../system/intel-gpu.nix
    ../system/niri.nix
    ../system/pcie-aspm.nix
    ../system/plymouth.nix
    ../system/power-management.nix
    ../system/sway.nix
    ../system/wireless.nix

    # Programs
    ../programs/appimage.nix

    # Services
    ../services/flatpak.nix
    ../services/geoclue2.nix
    ../services/greetd.nix
    ../services/node-exporter.nix
    ../services/podman.nix
    ../services/upower.nix
  ];

  hardware.uinput.enable = true;
}
