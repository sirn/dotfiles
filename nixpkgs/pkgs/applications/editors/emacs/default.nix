{ lib, pkgs }:

{
  emacsNativeComp-nox = pkgs.emacs-nox.override {
    nativeComp = true;
  };
}
