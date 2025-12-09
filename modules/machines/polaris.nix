{ lib, config, pkgs, ... }:

{
  flatpak.enable = true;
  machine.isLaptop = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/breeze.nix
    ../home/breeze-dark.nix
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/i18n.nix
    ../home/niri.nix
    ../home/sway.nix
    ../home/uwsm.nix

    # programs
    ../programs/1password.nix
    ../programs/brightnessctl.nix
    ../programs/ffmpeg.nix
    ../programs/firefox.nix
    ../programs/imagemagick.nix
    ../programs/intellij.nix
    ../programs/mpv.nix
    ../programs/sublime-text.nix
    ../programs/virt-manager.nix
    ../programs/yt-dlp.nix

    # services
    ../services/dropbox-symlinks.nix
  ];

  services.kanshi = lib.mkIf config.services.kanshi.enable {
    settings = [
      {
        output = {
          alias = "internal";
          criteria = "Samsung Display Corp. ATNA40HQ02-0  Unknown";
          mode = "2880x1800@120Hz";
          scale = 1.75;
        };
      }
      {
        profile = {
          name = "only_internal";
          outputs = [
            { criteria = "$internal"; }
          ];
        };
      }
      {
        profile = {
          name = "dual_aw3225qf_internal";
          outputs = [
            { criteria = "$aw3225qf"; }
            {
              criteria = "$internal";
              position = "457,1440";
            }
          ];
        };
      }
    ];
  };
}
