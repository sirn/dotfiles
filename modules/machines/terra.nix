{
  flatpak.enable = true;

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

    # programs
    ../programs/1password.nix
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

  services.kanshi.settings = [
    {
      profile = {
        name = "aw3225qf_ipmi";
        outputs = [
          { criteria = "$aw3225qf"; }
          {
            criteria = "Unknown Unknown Unknown";
            status = "disable";
          }
        ];
      };
    }
  ];
}
