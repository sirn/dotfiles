{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ local.asdcontrol ];
}
