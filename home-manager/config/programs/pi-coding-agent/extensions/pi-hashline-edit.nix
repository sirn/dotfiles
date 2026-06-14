{ lib, pkgs, ... }:

{
  home.file = {
    ".pi/agent/extensions/rimuruw-pi-hashline-edit".source = pkgs.local.pi-hashline-edit;
  };
}
