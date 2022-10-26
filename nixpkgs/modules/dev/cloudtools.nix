{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    cloudflared
    doctl
    google-cloud-sdk
    linode-cli
  ];
}
