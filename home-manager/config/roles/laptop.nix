{
  flatpak.enable = true;

  imports = [
    # Common
    ../common.nix
    ../common-linux.nix

    # Home
    ../home/breeze/shell.nix
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/i18n.nix
    ../home/laptop.nix
    ../home/niri/shell.nix
    ../home/sway/shell.nix
    ../home/uwsm.nix

    # Programs
    ../programs/ghostty.nix
    ../programs/helium.nix
    ../programs/imagemagick.nix
    ../programs/obsidian.nix
    ../programs/sublime-text.nix

    # Services
    ../services/coord.nix
    ../services/handsfree.nix
  ];
}
