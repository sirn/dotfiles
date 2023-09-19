{ config, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    package = pkgs.openssh;
  };
}
