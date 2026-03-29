{ pkgs, ... }:

{
  home.packages = with pkgs; [ unstable.jjui ];
}
