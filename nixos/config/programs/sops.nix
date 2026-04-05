{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ sops ];
}
