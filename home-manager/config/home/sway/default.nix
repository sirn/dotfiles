{
  imports = [
    ./base.nix

    ../../programs/fuzzel.nix
    ../../programs/waybar.nix
    ../../programs/wl-clipboard.nix

    ../../services/awww.nix
    ../../services/mako.nix
    ../../services/sway-audio-idle-inhibit.nix
    ../../services/swayidle.nix
    ../../services/swaylock.nix
    ../../services/wlsunset.nix
  ];
}
