{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    qemu
  ];
}
