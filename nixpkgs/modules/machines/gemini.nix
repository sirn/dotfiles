{
  imports = [
    ../profiles/common.nix
    ../profiles/graphical.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/xlocate.nix
  ];
}
