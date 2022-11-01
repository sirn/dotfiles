{ pkgs, ... }:

{
  machine.gui.enable = true;
  machine.flatpak.enable = true;
  runit.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/dev.nix
    ../profiles/graphical.nix
    ../profiles/multimedia.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/xlocate.nix
  ];
}
