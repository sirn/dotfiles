{
  imports = [
    ../../programs/sway.nix
    ../../programs/wl-clipboard.nix
    ../../programs/xdg-portal.nix

    # Services
    ../../services/kanshi.nix
    ../../services/sway-audio-idle-inhibit.nix
    ../../services/kwallet.nix
    ../../services/udiskie.nix
  ];
}
