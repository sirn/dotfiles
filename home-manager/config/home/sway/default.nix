{
  imports = [
    ./base.nix

    ../../programs/fuzzel.nix
    ../../programs/waybar.nix

    ../../services/awww.nix
    ../../services/mako.nix
    ../../services/swayidle.nix
    ../../services/swaylock.nix
    ../../services/wlsunset.nix
  ];
}
