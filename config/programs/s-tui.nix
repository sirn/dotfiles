{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    (s-tui.overrideAttrs (orig: rec {
      propagatedBuildInputs = orig.propagatedBuildInputs ++ [
        stress-ng
      ];
    }))
  ];
}
