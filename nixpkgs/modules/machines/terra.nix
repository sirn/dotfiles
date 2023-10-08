{
  machine.gui.enable = true;

  imports = [
    ../common.nix

    # profile
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/runit.nix
    ../home/sway.nix

    # programs
    ../programs/foot.nix
    ../programs/fuzzel.nix
    ../programs/mpv.nix
    ../programs/waybar.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/syncthing.nix
    ../runit/xlocate.nix
  ];
}
