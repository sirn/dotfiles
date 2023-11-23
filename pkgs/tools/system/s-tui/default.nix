{ lib, pkgs }:

pkgs.s-tui.overrideAttrs (orig: rec {
  propagatedBuildInputs = with pkgs; orig.propagatedBuildInputs ++ [
    stress-ng
  ];
})
