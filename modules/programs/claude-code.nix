{ pkgs, ... }:

{
  home.packages = [
    pkgs.local.claude-code
  ];
}
