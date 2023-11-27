{ pkgs
, emacs29
, emacs29-nox
, emacs29-macport
, ...
}:

{
  emacs = emacs29;

  emacs-nox = emacs29-nox.override {
    withNativeCompilation = true;
  };

  emacs-pgtk = emacs29.override {
    withNativeCompilation = true;
    withGTK2 = false;
    withGTK3 = true;
    withPgtk = true;
  };

  emacs-lucid = emacs29.override {
    withNativeCompilation = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  emacs-macport = emacs29-macport.override {
    withNativeCompilation = true;
  };
}
