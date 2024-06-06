{ pkgs, ... }:

{
  home = {
    sessionVariables = {
      QT_SCALE_FACTOR_ROUNDING_POLICY = "RoundPreferFloor";
    };

    pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
      package = pkgs.breeze-qt5;
      name = "breeze_cursors";
      size = 24;
    };
  };
}
