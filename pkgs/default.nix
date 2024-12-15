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

  inherit (recurseIntoAttrs (callPackage ./by-name/emacs { }))
    emacs
    emacs-nox
    emacs-pgtk
    emacs-lucid
    emacs-macport;

  emacsPackages = {
    apheleia = (callPackage ./by-name/emacs/elisp-packages/apheleia { });

    sqlite3 = (callPackage ./by-name/emacs/elisp-packages/sqlite3 { });

    visual-regexp-steroids = (callPackage ./by-name/emacs/elisp-packages/visual-regexp-steroids { });
  };

  firefox-csshacks = (callPackage ./by-name/firefox/csshacks.nix { });

  looking-glass-client_b6 = (callPackage ./by-name/looking-glass-client/b6.nix { });

  looking-glass-client_b7 = (callPackage ./by-name/looking-glass-client/b7.nix { });

  s-tui = (callPackage ./by-name/s-tui { });

  unison-nox = (callPackage ./by-name/unison-nox { });

  wl-clipboard = (callPackage ./by-name/wl-clipboard/wrapped.nix { });
}
