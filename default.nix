{ }:
import <nixpkgs> {
  overlays = [
    (final: prev: { local = (import ./pkgs final prev { }).${final.stdenv.hostPlatform.system}; })
  ];
}
