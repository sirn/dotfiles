{ pkgs, ... }:

{
  home = {
    sessionVariables = {
      QT_SCALE_FACTOR_ROUNDING_POLICY = "RoundPreferFloor";
    };

    pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
      package = pkgs.kdePackages.breeze;
      name = "breeze_cursors";
      size = 24;
    };
  };

  programs.firefox.nativeMessagingHosts = [
    pkgs.kdePackages.plasma-browser-integration
  ];
}
