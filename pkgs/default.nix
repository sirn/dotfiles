{ lib, pkgs, ... }:

let
  inherit (lib) callPackageWith recurseIntoAttrs;

  callPackage = callPackageWith (pkgs);

  callPackageUnstable = callPackageWith (pkgs.unstable);
in
{
  inherit (recurseIntoAttrs (callPackage ./data/fonts/ia-fonts { }))
    ia-writer-duo-static
    ia-writer-mono-static
    ia-writer-quattro-static;

  inherit (recurseIntoAttrs (callPackage ./applications/editors/emacs { }))
    emacs
    emacs-nox
    emacs-pgtk
    emacs-lucid
    emacs-macport;

  emacsPackages = {
    apheleia = (callPackage ./applications/editors/emacs/elisp-packages/apheleia { });

    sqlite3 = (callPackage ./applications/editors/emacs/elisp-packages/sqlite3 { });

    visual-regexp-steroids = (callPackage ./applications/editors/emacs/elisp-packages/visual-regexp-steroids { });
  };

  localias = (callPackage ./development/web/localias { });

  unison-nox = (callPackage ./applications/networking/sync/unison-nox { });

  s-tui = (callPackage ./tools/system/s-tui { });

  wl-clipboard = (callPackage ./tools/wayland/wl-clipboard/wrapped.nix { });
}
