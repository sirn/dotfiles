{ pkgs, ... }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs);

in
{
  ia-writer-duo-static = (callPackage ./data/fonts/ia-fonts { }).ia-writer-duo-static;
  ia-writer-mono-static = (callPackage ./data/fonts/ia-fonts { }).ia-writer-mono-static;
  ia-writer-quattro-static = (callPackage ./data/fonts/ia-fonts { }).ia-writer-quattro-static;
  droid-sans-thai = (callPackage ./data/fonts/droid-thai-fonts { }).droid-sans-thai;
  droid-serif-thai = (callPackage ./data/fonts/droid-thai-fonts { }).droid-serif-thai;
}
