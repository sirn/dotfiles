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
    aidermacs = (callPackage ./by-name/emacs/elisp-packages/aidermacs { });

    sqlite3 = (callPackage ./by-name/emacs/elisp-packages/sqlite3 { });
  };

  firefox-csshacks = (callPackage ./by-name/firefox/csshacks.nix { });

  looking-glass-client_b6 = (callPackage ./by-name/looking-glass-client/b6.nix { });

  looking-glass-client_b7 = (callPackage ./by-name/looking-glass-client/b7.nix { });

  wl-clipboard = (callPackage ./by-name/wl-clipboard/wrapped.nix { });
}
