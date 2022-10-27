{ lib, pkgs }:

{
  emacsNativeComp-nox = pkgs.emacs-nox.override {
    nativeComp = true;
  };

  emacsNativeComp-lucid = pkgs.emacs.override {
    nativeComp = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  emacsNativeComp-macport = pkgs.emacsMacport.override {
    nativeComp = true;
  };
}
