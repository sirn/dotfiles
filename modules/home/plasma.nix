{ pkgs, ... }:

{
  imports = [
    ../programs/ark.nix
    ../programs/gwenview.nix
    ../programs/okular.nix
  ];

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

  programs.firefox.nativeMessagingHosts = [
    pkgs.kdePackages.plasma-browser-integration
  ];
}
