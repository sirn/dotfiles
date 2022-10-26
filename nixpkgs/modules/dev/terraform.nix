{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    terraform
    terragrunt
  ];
}
