{
  machine.gui.enable = true;

  imports = [
    ../common.nix

    # profile
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/runit.nix

    # programs
    ../programs/emacsc.nix
    ../programs/foot.nix
    ../programs/fuzzel.nix
    ../programs/intellij.nix
    ../programs/kanshi.nix
    ../programs/looking-glass-client.nix
    ../programs/mpv.nix
    ../programs/sway.nix
    ../programs/waybar.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/syncthing.nix
    ../runit/xlocate.nix
  ];
}
