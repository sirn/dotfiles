{ pkgs, ... }:

{
  home.packages = with pkgs; [
    local.repoman
  ];
}
