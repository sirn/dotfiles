{ lib, pkgs }:

{
  emacsNativeComp-nox = pkgs.emacs29-nox.override {
    withNativeCompilation = true;
  };

  emacsNativeComp-pgtk = pkgs.emacs29.override {
    withNativeCompilation = true;
    withGTK2 = false;
    withGTK3 = true;
    withPgtk = true;
  };

  emacsNativeComp-lucid = pkgs.emacs29.override {
    withNativeCompilation = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  emacsNativeComp-macport = pkgs.emacs29-macport.override {
    withNativeCompilation = true;
  };
}
