{ lib, pkgs }:

{
  emacsNativeComp-nox = pkgs.emacs-nox.override {
    withNativeCompilation = true;
  };

  emacsNativeComp-pgtk = pkgs.emacs.override {
    withNativeCompilation = true;
    withGTK2 = false;
    withGTK3 = true;
    withPgtk = true;
  };

  emacsNativeComp-lucid = pkgs.emacs.override {
    withNativeCompilation = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  emacsNativeComp-macport = pkgs.emacsMacport.override {
    withNativeCompilation = true;
  };
}
