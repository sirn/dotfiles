{
  machine.gui.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/dev.nix
    ../profiles/graphical.nix
    ../profiles/multimedia.nix

    # machine-specific
    ../home/runit.nix
    ../home/flatpak.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/xlocate.nix
  ];
}