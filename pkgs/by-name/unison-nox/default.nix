{ lib, pkgs }:

pkgs.unison.override {
  enableX11 = false;
}
