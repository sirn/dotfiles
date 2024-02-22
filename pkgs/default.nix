{ lib, pkgs, ... }:

let
  inherit (lib) callPackageWith recurseIntoAttrs;

  callPackage = callPackageWith (pkgs);
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
    sqlite3 = (callPackage ./applications/editors/emacs/elisp-packages/sqlite3 { });

    visual-regexp-steroids = (callPackage ./applications/editors/emacs/elisp-packages/visual-regexp-steroids { });
  };

  unison-nox = (callPackage ./applications/networking/sync/unison-nox { });

  s-tui = (callPackage ./tools/system/s-tui { });

  wl-clipboard = (callPackage ./tools/wayland/wl-clipboard/wrapped.nix { });

  xdg-desktop-portal-kde = (callPackage ./desktop/plasma-5/xdg-desktop-portal-kde.nix { });
}
