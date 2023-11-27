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

  emacsNativeComp-nox = (callPackage ./applications/editors/emacs { }).emacsNativeComp-nox;
  emacsNativeComp-pgtk = (callPackage ./applications/editors/emacs { }).emacsNativeComp-pgtk;
  emacsNativeComp-lucid = (callPackage ./applications/editors/emacs { }).emacsNativeComp-lucid;
  emacsNativeComp-macport = (callPackage ./applications/editors/emacs { }).emacsNativeComp-macport;
  emacsPackages = {
    sqlite3 = (callPackage ./applications/editors/emacs/elisp-packages/sqlite3 { });
  };

  unison-nox = (callPackage ./applications/networking/sync/unison-nox { });
  s-tui = (callPackage ./tools/system/s-tui { });
}
