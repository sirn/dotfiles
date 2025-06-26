{ lib, pkgs, ... }:

let
  inherit (lib) callPackageWith recurseIntoAttrs;

  callPackage = callPackageWith (pkgs);

  callPackageUnstable = callPackageWith (pkgs.unstable);
in
{
  inherit (recurseIntoAttrs (callPackage ./by-name/ia-fonts { }))
    ia-writer-duo-static
    ia-writer-mono-static
    ia-writer-quattro-static;

  emacsPackages = {
    sqlite3 = (callPackage ./by-name/emacs/elisp-packages/sqlite3 { });
  };

  firefox-csshacks = (callPackage ./by-name/firefox/csshacks.nix { });

  wl-clipboard = (callPackage ./by-name/wl-clipboard/wrapped.nix { });
}
