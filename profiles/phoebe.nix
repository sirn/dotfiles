{
  nixos = {
    imports = [
      ../nixos/config/common.nix
      ../nixos/config/common-zfs.nix

      # profiles
      ../nixos/config/system/bluetooth.nix
      ../nixos/config/system/desktop.nix
      ../nixos/config/system/game.nix
      ../nixos/config/system/intel-gpu.nix
      ../nixos/config/system/plasma.nix
      ../nixos/config/system/wireless.nix

      # programs
      ../nixos/config/programs/1password.nix

      # services
      ../nixos/config/services/flatpak.nix
      ../nixos/config/services/node-exporter-ipmitool.nix
      ../nixos/config/services/node-exporter.nix
      ../nixos/config/services/podman.nix
      ../nixos/config/services/prometheus-agent.nix
      ../nixos/config/services/sddm.nix
    ];

    networking.hostId = "4d91fe14";
    networking.hostName = "phoebe";
  };

  home = {
    flatpak.enable = true;

    imports = [
      # common
      ../home-manager/config/common.nix
      ../home-manager/config/common-linux.nix

      # profiles
      ../home-manager/config/home/breeze-dark.nix
      ../home-manager/config/home/breeze.nix
      ../home-manager/config/home/flatpak.nix
      ../home-manager/config/home/fonts.nix
      ../home-manager/config/home/i18n.nix
      ../home-manager/config/home/niri
      ../home-manager/config/home/sway
      ../home-manager/config/home/uwsm.nix

      # programs
      ../home-manager/config/programs/1password.nix
      ../home-manager/config/programs/ffmpeg.nix
      ../home-manager/config/programs/imagemagick.nix
      ../home-manager/config/programs/intellij.nix
      ../home-manager/config/programs/mpv.nix
      ../home-manager/config/programs/sublime-text.nix
      ../home-manager/config/programs/virt-manager.nix
      ../home-manager/config/programs/ghostty.nix
      ../home-manager/config/programs/yt-dlp.nix
    ];
  };
}
