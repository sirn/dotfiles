{ pkgs, ... }:

{
  programs.nix-index = {
    enable = true;
  };

  home.packages = [ pkgs.comma ];
}
