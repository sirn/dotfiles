{ pkgs, ... }:

{
  home.packages = with pkgs; [
    coreutils
  ];

  home.shellAliases = {
    ls = "ls --color";
    ll = "ls -al";
  };
}
