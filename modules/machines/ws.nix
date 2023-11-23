{ config, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  machine.nixos.enable = true;

  imports = [
    ../common.nix

    # programs
    ../programs/emacsc.nix
    ../programs/s-tui.nix

    # services
    ../services/gpg-agent.nix
  ];
}
