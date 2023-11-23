{ config, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  machine.nixos.enable = true;

  imports = [
    ../common.nix
  ];
}
