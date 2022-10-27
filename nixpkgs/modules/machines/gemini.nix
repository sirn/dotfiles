{ config, ... }:

{
  config.machine.gui.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/dev.nix
    ../profiles/graphical.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/xlocate.nix
  ];
}
